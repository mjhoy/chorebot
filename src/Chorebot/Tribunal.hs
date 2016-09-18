module Chorebot.Tribunal where

import Chorebot.Assignment
import Chorebot.Profile

import Data.Time
import Data.List
import Data.List.Extra
import Data.Maybe

-- Rank a set of assignments.
rank :: [Profile] ->
        [Assignment] ->
        Double
rank profiles newAssignments = desirable + equitable
  where getProf a = fromJust $ find (\p -> (assignmentDoer a == profDoer p)) profiles
        profsAs = zip (map getProf newAssignments) newAssignments
        desirable = foldl' (\s (p,a) -> s + rankDesirable p a) 0 profsAs
        equitable = rankEquitable $ groupSort profsAs

-- Rank the overall equitability of assignment distribution.
--
-- Take the average difficulty assigned to each profile, and sum the
-- variance from this average.
rankEquitable :: [(Profile, [Assignment])] ->
                 Double
rankEquitable profsAs = sum variances
  where variances  = map (\d -> abs $ (fromIntegral d) - average) totalDiffs
        average    = (fromIntegral $ sum totalDiffs) / (fromIntegral $ length profsAs)
        totalDiff (_, assigns) = sum $ map assignmentDiff assigns
        totalDiffs = map totalDiff profsAs

-- Rank an assignment by its desirability.
--
-- An assignment gets some ranking >= 0, where a higher ranking means
-- this assignment is less desirable. The equation is something like:
--
--   chore difficulty / days since last assignment
--
-- if a chore has never been assigned to this profile, it receives a
-- 0, meaning it's an ideal assignment. it also receives a 0 if it is
-- permanently assigned.
rankDesirable :: Profile ->    -- profile of doer to be assigned
                 Assignment -> -- new assignment
                 Double
rankDesirable profile assignment = if not assigned && daysSinceLastAssigned > 0
                                   then assignedDifficulty / daysSinceLastAssigned
                                   else 0
  where
    assigned = isPermanentlyAssigned (profDoer profile) (assignmentChore assignment)

    assignedDifficulty :: Double
    assignedDifficulty = fromIntegral $ assignmentDiff assignment

    daysSinceLastAssigned :: Double
    daysSinceLastAssigned =
      case find sameChore sortedAssignments of
        Just a0 -> let tdiffs = realToFrac $ diffUTCTime (assignmentDate assignment) (assignmentDate a0)
                       tdiffd = ((tdiffs / 60) / 60) / 24
                   in tdiffd
        Nothing -> -1

    sameChore a = assignmentChore a == assignmentChore assignment

    sortedAssignments = sortBy (\a b -> (assignmentDate a) `compare` (assignmentDate b)) (profAssignments profile)
