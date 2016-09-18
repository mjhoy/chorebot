module Chorebot.Profile
       ( Profile
       , profDoer
       , profAssignments
       , latestChores
       , buildProfile
       , printProfile
       , difficultyPerDay
       ) where

import Chorebot.Doer
import Chorebot.Chore
import Chorebot.Assignment

import Text.Printf
import Data.List
import Data.Time

-- A profile is simply a doer associated with their assigned chore
-- history. And from this we can extrapolate interesting information.
data Profile = Profile { profDoer :: Doer,
                         profAssignments :: [Assignment]
                       } deriving (Eq, Show)

instance Ord Profile where
  p1 `compare` p2 = (profDoer p1) `compare` (profDoer p2)

buildProfile :: [Assignment] -> -- List of all/any chore assignments
                Doer ->
                Profile
buildProfile assigns doer' = Profile doer' assignments''
  where
    assignments'' = sortBy (\a b -> (assignmentDate a) `compare` (assignmentDate b)) assignments'
    assignments' = filter byDoer assigns
    byDoer a = (assignmentDoer a) == doer'

-- helper function
padString :: String -> Integer -> String
padString (c:cs) i
  | i > 0 = c : (padString cs (i - 1))
  | otherwise = ""
padString "" i
  | i > 0 = ' ' : (padString "" (i - 1))
  | otherwise = ""

secondsInDay :: Int
secondsInDay = 60 * 60 * 24

latestChores :: Profile ->
                [Chore]
latestChores (Profile _ []) = []
latestChores (Profile _ as@(a:_)) =
    let latest = foldl' lateDate (assignmentDate a) as
    in map assignmentChore $ filter (\a' -> (assignmentDate a') == latest) as
  where
    lateDate :: UTCTime -> Assignment -> UTCTime
    lateDate t a' = let t' = assignmentDate a'
                   in if t' > t then t' else t

difficultyPerDay :: UTCTime ->  -- the current time
                    Profile ->
                    Double
difficultyPerDay now (Profile _doer as) =
  -- get the earliest date in `as'
    let earliest  = foldl' earlyDate now as
        diffTime  = max secondsInDay $ round $ diffUTCTime now earliest
        daysSince :: Double
        daysSince = fromIntegral diffTime / fromIntegral secondsInDay
        totalDifficulty = foldl' (\diff a -> diff + (assignmentDiff a)) 0 as
    in fromIntegral totalDifficulty / daysSince
  where
    earlyDate :: UTCTime -> Assignment -> UTCTime
    earlyDate t a = let t' = assignmentDate a
                    in if t' < t then t' else t

-- for the format:
--
--   name         diff/day  prev chores
--   --------------------------------------------------
--   Bilbo Baggi  5         sweep-basement, email-phone
--
printProfile :: UTCTime -> -- current time
                Profile ->
                String
printProfile now prof =
  (padString (doerName (profDoer prof)) 12) ++ " " ++
  (padString (printf "%.2f" (difficultyPerDay now prof)) 9) ++ " " ++
  (concat $ intersperse ", " $ map choreIdent (latestChores prof))
