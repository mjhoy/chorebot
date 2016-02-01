{-# LANGUAGE BangPatterns #-}

module Chorebot.Distributor where

import Data.Time
import Data.List

import System.Random

import Chorebot.Doer
import Chorebot.Chore
import Chorebot.Assignment
import Chorebot.Profile

-- Calculate permanent assignments.
assignPermanentChores :: ([Chore],[Assignment]) -> -- chores to assign, current assignments
                         [Doer] ->                 -- doers to assign to
                         UTCTime ->                -- current date
                         ([Chore],[Assignment])    -- remaining chores, assignments + permanent assignments
assignPermanentChores ([],  assignments) _     _ = ([], assignments)
assignPermanentChores (rm,  assignments) []    _ = (rm, assignments)
assignPermanentChores (rm,  assignments) doers t = (rm', assignments')
  where
    (rm', assignments') = foldl' assignPermForDoer (rm, assignments) doers
    assignPermForDoer ([], as) _ = ([], as)
    assignPermForDoer (cs, as) d =  (cs \\ assignedChores,
                                    as ++ (map (assign d t) assignedChores))
      where
        assignedChores   = foldl' assignPerm [] cs
        assignPerm acc c = if isPermanentlyAssigned d c
                           then c:acc
                           else acc

-- Given a list of chores, current assignments, a profile (with past
-- assignments), the time, a sanity check counter, make an assignment!
mkAssignment :: ([Chore], [Assignment], Int) -> -- remaining chores to assign,
                                                -- current assignments,
                                                -- current sanity check

                Profile ->                      -- profile to assign to

                Int ->                          -- sanity check limit

                UTCTime ->                      -- current time

                ([Chore], [Assignment], Int)    -- remaining chores after this assignment,
                                                -- current assignments plus this assignment,
                                                -- sanity check incremented by 1
mkAssignment (c, a, s) p limit n = mkAssignment' (c, a, s) p n []
  where
    mkAssignment' ([], assignments, sc) _ _ acc = (acc, assignments, sc + 1)
    mkAssignment' (chore:cs, assignments, sc) prof t acc =
      let doer = pdoer prof
          newAssignment = assign doer t chore

          -- logic about making an assignment for a particular chore.
          -- either the sanity check limit is reached (and the chore
          -- is assigned no matter what), or the chore: 1. is not in
          -- doer's vetoes, and, 2. is not in the doer's most recent
          -- past assignments
          shouldAssign = or [
            sc >= limit,
            (and [ (not $ hasVetoed doer chore),
                   (not $ elem chore $ latestChores prof) ])
            ]

      in if shouldAssign
         then (acc ++ cs, newAssignment:assignments, sc + 1)
         else mkAssignment' (cs, assignments, sc) prof t (chore:acc)

-- distribute the chores! returns a list of new assignments plus a
-- flag about whether we reached the "sanity check" limit, meaning
-- that chores had to be forced assigned (possibility: vetoes
-- precluded one chore or another from being assigned)
distribute :: RandomGen g =>

              -- list of profiles to assign chores to
              [Profile] ->

              -- list of possible chores to assign
              [Chore] ->

              -- list of past chore assignments
              [Assignment] ->

              -- current time
              UTCTime ->

              -- random number generator
              g ->

              -- a list of new assignments plus whether any chores
              -- were force assigned, plus a new random gen
              ([Assignment], Bool, g)

distribute profiles chores pastAssignments now gen = (finalAssignments, didForceAssign, gen'')

  where

    -- step 1: remove chores that have been assigned within the
    -- required chore interval
    chores1 = filter choreNeedsAssignment chores

    -- sort the past assignments most recent first
    sortedPastAssignments =
      let cmpDates a1 a2 = date a1 `compare` date a2
      in reverse $ sortBy cmpDates pastAssignments

    -- determine whether a chore needs to be done
    choreNeedsAssignment :: Chore -> Bool
    choreNeedsAssignment c =
      let prevAssignment = find (\a' -> c == (chore a')) sortedPastAssignments
      in case prevAssignment of

        -- a' is the previous assignment of chore c.
        --
        -- calculate whether the time since last defined is greater
        -- than the interval.
        Just a' -> let diff = diffUTCTime now (date a')
                       secInDay = 24 * 60 * 60
                       intervalSeconds = fromIntegral $ (7 * interval c) * secInDay
                   in diff >= intervalSeconds

        -- chore c has never been assigned before, so we should
        -- definitely assign it.
        Nothing -> True

    -- step 2: distribute permanent chores
    (chores2, assignments1) = distributePerm profiles chores1 []

    -- helper function: generate n random numbers and return the list
    -- and the new random generator
    randomNRs n g = foldl' fn ([], g) (take n $ repeat ())
      where fn (acc,g') _ = let (a, g'') = randomR (1,10000) g'
                            in (a:acc, g'')

    -- step 3: sort chores by difficulty, hardest *first*. chores of
    -- equal difficulty are randomly sorted.
    (chores3, gen') =
      let (rs, lgen') = randomNRs (length chores2) gen
          cRandomWeight = zip rs chores2
          sortFn :: (Int, Chore) -> (Int, Chore) -> Ordering
          sortFn (r1, c1) (r2, c2) = case difficulty c1 `compare` difficulty c2 of
            EQ -> r1 `compare` r2
            a  -> a
          in ((map snd $ reverse $ sortBy sortFn cRandomWeight), lgen')

    -- step 4: sort the profiles in order of least "difficultyPerDay"
    -- first; i.e., those profiles who have done, on average, the
    -- least work should get the first assignments. equal values are
    -- randomly sorted, as with chore difficulty.
    (profiles2, gen'') =
      let (rs, lgen'') = randomNRs (length profiles) gen'
          pRandomWeight = zip rs profiles
          sortFn :: (Int, Profile) -> (Int, Profile) -> Ordering
          sortFn (r1, p1) (r2, p2) = case difficultyPerDay now p1 `compare` difficultyPerDay now p2 of
            EQ -> r1 `compare` r2
            a -> a
      in ((map snd $ sortBy sortFn pRandomWeight), lgen'')

    -- step 5: distribute the rest of the chores
    (assignments3, didForceAssign) = distributeAll profiles2 chores3 assignments1 0

    finalAssignments = assignments3

    -- an upper limit on iteration: don't try to assign chores more
    -- than `sanityCheckLimit` times
    sanityCheckLimit = ((length profiles) * (length chores)) + 50

    -- distribute permanent assignments
    distributePerm ps cs acc =
      let doers = map pdoer ps
      in assignPermanentChores (cs, acc) doers now

    -- distribute chores
    distributeAll [] _ _ _  = ([], False) -- no profiles

    distributeAll ps c a s = repeatedDist c a s False

      where

        -- repeatedly distribute chores until none remain.

        repeatedDist [] acc sc _
          | sc >= sanityCheckLimit = (acc, True)
          | otherwise = (acc, False)

        repeatedDist cs acc sc reord = repeatedDist cs' acc' sc' True
          where
            (!cs', !acc', !sc') = foldl' mkAssignment' (cs, acc, sc) ps'
            mkAssignment' x y = mkAssignment x y sanityCheckLimit now
            ps' = case reord of
              False -> ps
              True -> map snd $ sortBy fstSort $ zip (map curDifficulty ps) ps
            fstSort (f1,_) (f2,_) = f1 `compare` f2
            curDifficulty :: Profile -> Int
            curDifficulty prof = let d = (pdoer prof)
                                     as = filter (\a' -> (doer a') == d) acc
                                 in sum (map adiff as)
