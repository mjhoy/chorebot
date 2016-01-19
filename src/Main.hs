import System.Environment
import System.IO
import System.Exit
import System.Directory
import ChoreParser
import Chore
import DoerParser
import Doer
import AssignmentParser
import Assignment

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

  args <- getArgs
  case args of
    ("list-chores":_) ->
      mapM_ (putStrLn . printChore) chores
    ("list-doers":_) ->
      mapM_ (putStrLn . printDoer) doers
    ("list-assignment-history":_) ->
      putStr $ printAssignments assignments
    _ -> do
      putErr "unknown action"
      exitFailure
