module Main (main) where

import System.Environment
import System.IO
import System.Exit
import System.Directory
import System.Random

import Chorebot.Chore
import Chorebot.Chore.Parser
import Chorebot.Doer
import Chorebot.Doer.Parser
import Chorebot.Assignment
import Chorebot.Assignment.Parser
import Chorebot.Profile
import Chorebot.Distributor

import Data.Time

-- helper function
putErr :: String -> IO ()
putErr str = hPutStrLn stderr str

main :: IO ()
main = do
  let choresfn = "chores.txt"
      doersfn  = "doers.txt"
  chorestxt <- readFile choresfn
  doerstxt  <- readFile doersfn

  -- parse chores
  chores <- case runChoresParser choresfn chorestxt of
    Right chores -> return chores
    Left err -> do
      putErr err
      exitFailure

  -- parse doers
  doers <- case runDoersParser doersfn doerstxt of
    Right doers -> return doers
    Left err -> do
      putErr err
      exitFailure

  -- parse assignments
  let assignmentfn = "assignment-history.txt"
  assignmentsExists <- doesFileExist assignmentfn
  assignments <- do
    case assignmentsExists of
      True -> do
        assignmenttxt <- readFile assignmentfn
        case runAssignmentParser chores doers assignmentfn assignmenttxt of
          Right assignments -> return assignments
          Left err -> do
            putErr err
            exitFailure
      False -> return []

  let profiles = map (buildProfile assignments) doers

  args <- getArgs
  case args of
    ("--help":_) ->
      putErr $ "usage: chorebot COMMAND\n\n" ++
               "commands:\n" ++
               "  list-chores              List current chores\n" ++
               "  list-doers               List current chore-doers\n" ++
               "  list-assignment-history  List past chore assignments\n" ++
               "  list-profiles            List profile info\n" ++
               "  distribute               Make new chore assignments"
    ("list-chores":_) ->
      mapM_ (putStrLn . printChore) chores
    ("list-doers":_) ->
      mapM_ (putStrLn . printDoer) doers
    ("list-assignment-history":_) ->
      putStr $ printAssignments assignments
    ("list-profiles":_) -> do
      putStrLn "name         diff/day  prev chores"
      putStrLn "----------------------------------"
      t <- getCurrentTime
      mapM_ (putStrLn . (printProfile t)) profiles
    ("distribute":_) -> do
      t <- getCurrentTime
      gen <- getStdGen
      let (newAssignments, didForce) = distribute profiles chores assignments t gen
      case didForce of
        True -> do
          putStrLn "WARNING: sanity check flag flipped"
          putStr $ printAssignments newAssignments
        False -> putStr $ printAssignments newAssignments
    _ -> do
      putErr "unknown action (use --help for more information)"
      exitFailure
