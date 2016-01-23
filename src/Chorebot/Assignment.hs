module Chorebot.Assignment where

import Chorebot.Chore
import Chorebot.Doer

import Data.Time
import Data.Time.Format
import Data.List

data Assignment = Assignment { chore      :: Chore,
                               doer       :: Doer,
                               date       :: UTCTime,
                               adiff      :: Int
                             } deriving (Show, Eq)

instance Ord Assignment where
  (Assignment _ _ d _) `compare`
    (Assignment _ _ d' _) = d `compare` d'

printAssignments :: [Assignment] -> String
printAssignments assignments =
  concat $ intersperse "\n" datelines
  where assignments' = reverse $ sort assignments
        grouped = groupBy (\a b -> (assignmentDay a) == (assignmentDay b)) assignments'
        grouped' = map (groupBy (\a b -> (doer a) == (doer b))) grouped
        prcd ls@((x:_):_) = [ "[" ++ (assignmentDay x) ++ "]\n\n" ++ (concatMap printAssignmentsUser ls) ]
        prcd _ = []
        datelines = concatMap prcd grouped'
        printAssignmentsUser :: [Assignment] -> String
        printAssignmentsUser (x:xs) = (name d) ++ " <" ++ (email d) ++ ">: " ++ "\n" ++ (concatMap printAssignment (x:xs)) ++ "\n"
          where d = doer x
        printAssignment a =
          "  " ++ (title c) ++ " <" ++ (ident c) ++ "> " ++ (show $ adiff a) ++ "\n"
          where c = chore a

assignmentDay :: Assignment -> String
assignmentDay (Assignment _ _ d _) =
  formatTime defaultTimeLocale "%Y/%m/%d" d
