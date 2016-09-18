module Chorebot.Assignment where

import Chorebot.Chore
import Chorebot.Doer

import Data.Time
import Data.List
import Data.Maybe (isJust)

import Text.Regex (matchRegex)

data Assignment = Assignment { assignmentChore :: Chore,
                               assignmentDoer  :: Doer,
                               assignmentDate  :: UTCTime,
                               assignmentDiff  :: Int
                             } deriving (Show, Eq)

instance Ord Assignment where
  (Assignment _ _ d _) `compare`
    (Assignment _ _ d' _) = d `compare` d'

assign :: Doer -> UTCTime -> Chore -> Assignment
assign doer' date' chore' = Assignment chore' doer' date' (choreDifficulty chore')

chkPats :: [Pattern] -> Chore -> Bool
chkPats pats c = any chkPat pats
  where chkPat p = isJust $ matchRegex (patToRegex p) (choreIdent c)

isPermanentlyAssigned :: Doer -> Chore -> Bool
isPermanentlyAssigned d c = chkPats (doerPerm d) c

hasVetoed :: Doer -> Chore -> Bool
hasVetoed d c = chkPats (doerVetoes d) c

printAssignments :: [Assignment] -> String
printAssignments assignments =
  concat $ intersperse "\n" datelines
  where assignments' = reverse $ sort assignments
        grouped = groupBy (\a b -> (assignmentDay a) == (assignmentDay b)) assignments'
        grouped' = map (sortBy (\a b -> (doerEmail (assignmentDoer a)) `compare` (doerEmail (assignmentDoer b)))) grouped
        grouped'' = map (groupBy (\a b -> (assignmentDoer a) == (assignmentDoer b))) grouped'
        prcd ls@((x:_):_) = [ "[" ++ (assignmentDay x) ++ "]\n\n" ++ (concatMap printAssignmentsUser ls) ]
        prcd _ = []
        datelines = concatMap prcd grouped''
        printAssignmentsUser :: [Assignment] -> String
        printAssignmentsUser [] = ""
        printAssignmentsUser (x:xs) = (doerName d) ++ " <" ++ (doerEmail d) ++ ">: " ++ "\n" ++ (concatMap printAssignment (x:xs)) ++ "\n"
          where d = assignmentDoer x
        printAssignment a =
          "  " ++ (choreTitle c) ++ " <" ++ (choreIdent c) ++ "> " ++ (show $ assignmentDiff a) ++ "\n"
          where c = assignmentChore a

assignmentDay :: Assignment -> String
assignmentDay (Assignment _ _ d _) =
  formatTime defaultTimeLocale "%Y/%m/%d" d
