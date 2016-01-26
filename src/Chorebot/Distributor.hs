module Chorebot.Distributor where

import Data.Time
import Data.List

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
assignPermanentChores (rem, assignments) []    _ = (rem, assignments)
assignPermanentChores (rem, assignments) doers t = (rem', assignments')
  where
    (rem', assignments') = foldl' assignPermForDoer (rem, assignments) doers
    assignPermForDoer ([], as) _ = ([], as)
    assignPermForDoer (cs, as) d = (cs \\ assignedChores,
                                    as ++ (map (assign d t) assignedChores))
      where
        assignedChores   = foldl' assignPerm [] cs
        assignPerm acc c = if isPermanentlyAssigned d c
                           then c:acc
                           else acc

-- keep track of how many times we're looping through chores to
-- assign, if we go past this we will force assign chores even if they
-- are vetoed.
sanityCheckLimit :: [Chore] -> [Doer] -> Int
sanityCheckLimit cs ds = (length cs) * (length ds) + 50

-- Given a list of chores, current assignments, a profile (with past
-- assignments), the time, a sanity check counter, make an assignment!
mkAssignment :: Profile ->                      -- profile to assign to

                ([Chore], [Assignment], Int) -> -- remaining chores to assign,
                                                -- current assignments,
                                                -- current sanity check

                Int ->                          -- sanity check limit

                UTCTime ->                      -- current time

                ([Chore], [Assignment], Int)    -- remaining chores after this assignment,
                                                -- current assignments plus this assignment,
                                                -- sanity check incremented by 1
mkAssignment p (c, a, s) limit n = mkAssignment' p (c, a, s) n []
  where
    mkAssignment' _ ([], assignments, sc) _ acc = (acc, assignments, sc + 1)
    mkAssignment' prof (chore:cs, assignments, sc) t acc =
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
         else mkAssignment' prof (cs, assignments, sc) t (chore:acc)

-- distribute :: [Profile] ->
--               [Chore] ->
--               UTCTime ->        -- current time
--               [Assignment]
-- distribute p c t = distribute' p c' a'
--   where
--     (c', a') = foldl' assignPermanents (c, []) profiles
--     assignPermanents (cs, as) p = foldl' assignPermanent (cs, as) cs
--       where
--         assignPermanent (cs', as') chore = if isPermanentlyAssigned (pdoer p) chore
--                                            then (delete chore cs', as' ++ chore)
--                                            else (cs', as')
--     distribute' [] _ a = a
--     distribute' _ [] a = a
--     distribute' profiles remaining assigned =
--       let (remaining'', assigned'') = foldl' assign (remaining, assigned) profiles
--           assign (r,a) p = case find (canAssign p) r of
--             Just chore -> (delete chore r, a ++ (assign chore (pdoer p) t))
--             Nothing    -> (r,a)
--       in distribute' profiles remaining' assigned'
--       where
--         distribute'' p = 
