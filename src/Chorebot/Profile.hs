module Chorebot.Profile
       ( buildProfile
       , printProfile
       ) where

import Chorebot.Doer
import Chorebot.Chore
import Chorebot.Assignment

import Text.Printf
import Data.List
import Data.Time

-- A profile is simply a doer associated with their assigned chore
-- history. And from this we can extrapolate interesting information.
data Profile = Profile { pdoer :: Doer,
                         assignments :: [Assignment]
                       } deriving (Eq, Show)

buildProfile :: [Assignment] -> -- List of all/any chore assignments
                Doer ->
                Profile
buildProfile assignments doer' = Profile doer' assignments''
  where
    assignments'' = sortBy (\a b -> (date a) `compare` (date b)) assignments'
    assignments' = filter byDoer assignments
    byDoer a = (doer a) == doer'

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
latestChores (Profile d []) = []
latestChores (Profile d as@(a:_)) =
    let latest = foldl' lateDate (date a) as
    in map chore $ filter (\a -> (date a) == latest) as
  where
    lateDate :: UTCTime -> Assignment -> UTCTime
    lateDate t a = let t' = date a
                   in if t' > t then t' else t

difficultyPerDay :: UTCTime ->  -- the current time
                    Profile ->
                    Double
difficultyPerDay now (Profile d as) =
  -- get the earliest date in `as'
    let earliest  = foldl' earlyDate now as
        diffTime  = max secondsInDay $ round $ diffUTCTime now earliest
        daysSince :: Double
        daysSince = fromIntegral diffTime / fromIntegral secondsInDay
        totalDifficulty = foldl' (\d a -> d + (adiff a)) 0 as
    in fromIntegral totalDifficulty / daysSince
  where
    earlyDate :: UTCTime -> Assignment -> UTCTime
    earlyDate t a = let t' = date a
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
  (padString (name (pdoer prof)) 12) ++ " " ++
  (padString (printf "%.2f" (difficultyPerDay now prof)) 9) ++ " " ++
  (concat $ intersperse ", " $ map ident (latestChores prof))
