module Chorebot.Tribunal where

import Chorebot.Assignment
import Chorebot.Chore
import Chorebot.Doer
import Chorebot.Profile

import Data.Time
import Data.List
import Data.Maybe

-- Rank a set of assignments.
rank :: [Profile] ->
        [Assignment] ->
        Double
rank profiles newAssignments =
  let getProf a = fromJust $ find (\p -> (doer a == pdoer p)) profiles
      profsAs = zip (map getProf newAssignments) newAssignments
  in foldl' (\s (p,a) -> s + rankAssignment p a) 0 profsAs

-- An assignment gets some ranking >= 0, where a higher ranking means
-- this assignment is less desirable. The equation is something like:
--
--   chore difficulty / days since last assignment
--
-- if a chore has never been assigned to this profile, it receives a
-- 0, meaning it's an ideal assignment. it also receives a 0 if it is
-- permanently assigned.
rankAssignment :: Profile ->    -- profile of doer to be assigned
                  Assignment -> -- new assignment
                  Double
rankAssignment profile assignment = if not assigned && daysSinceLastAssigned > 0
                                    then assignedDifficulty / daysSinceLastAssigned
                                    else 0
  where
    assigned = isPermanentlyAssigned (pdoer profile) (chore assignment)

    assignedDifficulty :: Double
    assignedDifficulty = fromIntegral $ adiff assignment

    daysSinceLastAssigned :: Double
    daysSinceLastAssigned =
      case find sameChore sortedAssignments of
        Just a0 -> let tdiffs = realToFrac $ diffUTCTime (date assignment) (date a0)
                       tdiffd = ((tdiffs / 60) / 60) / 24
                   in tdiffd
        Nothing -> -1

    sameChore a = chore a == chore assignment

    sortedAssignments = sortBy (\a b -> (date a) `compare` (date b)) (assignments profile)
