{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chorebot.Distributor where

import Data.Time
import Data.List

import System.Random
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Extra
import Data.Maybe

import Chorebot.Doer
import Chorebot.Chore
import Chorebot.Assignment
import Chorebot.Profile

import Debug.Trace

data PendingChore = PendingChore { _pendingChore :: Chore
                                 , _assignedCount :: Int
                                 } deriving (Eq)

mkPending :: Chore -> PendingChore
mkPending chore = PendingChore chore (count chore)

decPending :: PendingChore -> Maybe PendingChore
decPending p | ac > 1 = Just (p { _assignedCount = ac - 1 })
             | otherwise = Nothing
  where ac = _assignedCount p

-- The state for our algorithm, which will be updated in
-- iterations. As chores are assigned, pending chores move from the
-- pending chores list into the new assignments list. In addition a
-- sanity check counter is incremented, in cases where chores are not
-- able to be assigned and we would loop forever.
data CState = CState
              { pendingChores :: [PendingChore]
              , newAssignments :: [Assignment]
              , sanityCheck :: Int
              , permAssignments :: [(Profile, Int)]
              }

-- Read-only configuration for the algorithm.
data CConf = CConf
             { time :: UTCTime
             , pastAssignments :: [Assignment]
             , profiles :: [Profile]
             , sanityCheckLimit :: Int
             }

-- Helper function to create the config. Make sure that the list of
-- assignments is sorted to begin with.
mkCConf :: UTCTime -> [Assignment] -> [Profile] -> Int -> CConf
mkCConf t a p sc = CConf t a' p sc
  where a' = let cmpDates a1 a2 = date a1 `compare` date a2
             in reverse $ sortBy cmpDates a

-- Our chore assignment monad. We want to capture state (the current
-- pending chores to assign and the assignments made so far),
-- read-only configuration (a list of profiles, the current time, a
-- limit of # of iterations, etc), and a random number generator that
-- introduces some randomness into sorting.
newtype C a = C { _runC :: RandT StdGen (ReaderT CConf (State CState)) a }
            deriving (Functor, Applicative, Monad,
                      MonadState CState,
                      MonadReader CConf,
                      MonadRandom)

-- Run the C monad. Given an initial config, random generator, and
-- state, run an action and return the result along with the new
-- generator and updated state. Used to run the overall action, which
-- is distribute'.
runC :: C a -> CConf -> CState -> StdGen -> ((a, StdGen), CState)
runC (C k) conf st gen = runIdentity (runStateT (runReaderT (runRandT k gen) conf) st)


-- Public-facing function. Delegate to the C monad to actually run the
-- algorithm. Set up our initial reader config and state.
distribute :: [Profile] -> [Chore] -> [Assignment] -> UTCTime -> StdGen -> ([Assignment], Bool, StdGen)
distribute profiles chores assignments now gen =
  let (((as, hitSc), gen'), _) = runC distribute' conf st gen
  in (as, hitSc, gen')
  where
    st = CState { pendingChores = map mkPending chores
                , newAssignments = []
                , sanityCheck = 0
                , permAssignments = [] }
    conf = mkCConf now assignments profiles sclimit
    sclimit = (length chores) * (length profiles) + 50

-- The distribution algorithm.
distribute' :: C ([Assignment], Bool)
distribute' = do
  removeUneccessaryChores -- step 1
  distributePerm          -- step 2
  sortChores              -- step 3
  hitLim <- distributeAll -- step 4
  st <- get
  return (newAssignments st, hitLim)

-- reader helpers
askTime             :: C UTCTime
askProfiles         :: C [Profile]
askPastAssignments  :: C [Assignment]
askSanityCheckLimit :: C Int

askTime = liftM time ask
askProfiles = liftM profiles ask
askPastAssignments = liftM pastAssignments ask
askSanityCheckLimit = liftM sanityCheckLimit ask

-- Step 1: Remove chores that have been assigned within the required
-- chore interval.
removeUneccessaryChores :: C ()
removeUneccessaryChores = do
  st <- get
  t <- askTime
  c' <- filterM choreNeedsAssignment (pendingChores st)
  let st' = st { pendingChores = c' }
  put st'
  return ()

choreNeedsAssignment :: PendingChore -> C Bool
choreNeedsAssignment (PendingChore c _) = do
  now <- askTime
  past <- askPastAssignments
  let prevAssignment = find (\a' -> c == (chore a')) past
  case prevAssignment of
    -- a' is the previous assignment of chore c.
    --
    -- calculate whether the time since last defined is greater
    -- than the interval.
    Just a' -> let diff = diffUTCTime now (date a')
                   secInDay = 24 * 60 * 60
                   intervalSeconds = fromIntegral $ (7 * interval c) * secInDay
               in return $ diff >= intervalSeconds
    Nothing -> return True


assignPerm :: Profile -> PendingChore -> C ()
assignPerm prof pending = do
  _ <- assignPending prof pending
  st <- get
  let pa = permAssignments st
      i' = case lookup prof pa of
        Just i  -> i + 1
        Nothing -> 1
  st <- put $ st { permAssignments = (prof,i'):pa }
  return ()

assignPending :: Profile -> PendingChore -> C Assignment
assignPending prof pending = do
  st <- get
  now <- askTime
  let doer' = pdoer prof
      c = _pendingChore pending
      assignment = assign doer' now c
      assignments' = assignment : (newAssignments st)
      chores' = mapMaybe (ifEqDecPending pending) (pendingChores st)

  put $ st { pendingChores = chores'
           , newAssignments = assignments' }
  return assignment
    where
      ifEqDecPending p1 p2 | p1 == p2 = decPending p2
                           | otherwise = Just p2

-- Step 2: distribute permanent chores.
distributePerm :: C ()
distributePerm = do
  profiles <- liftM profiles ask
  now <- askTime

  forM_ profiles $ \p -> do
    -- check the current pending chores that are permanently assigned
    -- to `p`.
    let doer' = pdoer p
    cs <- liftM pendingChores get
    let toAssign = filter (\pc -> doer' `isPermanentlyAssigned` (_pendingChore pc)) cs
    mapM_ (assignPerm p) toAssign

  return ()

-- Helper to generate a sequence of random integers (using an
-- arbitrary, "good enough" domain).
randomSequence :: Int -> C [Int]
randomSequence n = sequence $ replicate n $ getRandomR (1,10000)

randomSort :: [a] -> C [a]
randomSort as = do
  rs <- randomSequence (length as)
  let asW = zip rs as
  return $ map snd $ sortBy (\a b -> fst a `compare` fst b) asW

-- Step 3: Sort the pending chores randomly.
sortChores :: C ()
sortChores = do
    st <- get
    let chores = pendingChores st
    chores' <- randomSort chores
    put $ st { pendingChores = chores' }

-- Step 4: Distribute the remaining pending chores. Returns whether we
-- hit the sanity check limit or not.
distributeAll :: C Bool
distributeAll = do
    now <- askTime
    profiles <- askProfiles

    -- Randomize profiles.
    sortedProfiles <- randomSort profiles

    lim <- askSanityCheckLimit

    let checkIter :: C Bool
        checkIter = do
          st <- get
          let chores = pendingChores st
              sc = sanityCheck st
          if sc > lim || (length chores == 0)
            then return False
            else return True

    whileM $ do
      mapM_ distributeOne sortedProfiles
      checkIter

    st <- get
    let hitSanityCheck = if (sanityCheck st) > lim
                         then True
                         else False

    return hitSanityCheck

-- Take one pending chore and make a new assignment. Either remove the
-- chore from the pending chores list or decrease its count by 1 if
-- greater than 1. it is possible no chore assignment is
-- made. increase the sanity check counter by 1.
distributeOne :: Profile -> C ()
distributeOne profile = do
  lim <- askSanityCheckLimit
  now <- askTime
  st <- get
  let chores = pendingChores st
      assignments = newAssignments st
      doer' = pdoer profile
      sc = sanityCheck st
      permAssignmentCount = fromMaybe 0 $ lookup profile (permAssignments st)
      shouldAssign pending =
        let c = _pendingChore pending
        in or [ sc >= lim, -- force assignment if sanity check is above limit.
                not $ or [ (hasVetoed doer' c),
                           (elem c $ map chore (filter ((== (pdoer profile)) . doer) assignments)),
                           (elem c $ latestChores profile)
                         ]
              ]
      -- assign the first chore of `pendingChores' that makes sense to the
      -- doer.
      newChoreToAssign = find shouldAssign chores

  case permAssignmentCount of
    0 -> do
      case newChoreToAssign of
        -- we should assign `c` to `profile`
        Just pending -> assignPending profile pending >> return ()

        -- chore could not be assigned, noop
        Nothing -> return ()

    _ -> do
      -- skip this round and decrease perm assignment count
      put $ st { permAssignments = (profile,permAssignmentCount - 1):(permAssignments st) }

  incSc -- ensure sanity check counter is increased
  return ()

incSc :: C ()
incSc = do
  st <- get
  let sc = sanityCheck st
  put $ st { sanityCheck = sc + 1 }
