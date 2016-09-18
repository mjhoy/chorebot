module Main (main) where

import System.IO
import System.Exit
import System.Directory
import System.Random
import Data.Time
import Data.List
import Options.Applicative

import Text.Pandoc
import Text.Pandoc.Error (handleError)

import Chorebot.Chore
import Chorebot.Chore.Parser
import Chorebot.Doer
import Chorebot.Doer.Parser
import Chorebot.Assignment
import Chorebot.Assignment.Parser
import Chorebot.Profile
import Chorebot.Distributor
import Chorebot.Tribunal
import Chorebot.Time

-- helper function
putErr :: String -> IO ()
putErr string = hPutStrLn stderr string

-- data structure to hold the options passed to the `chorebot`
-- command.
data Cmd = Cmd { _date :: IO UTCTime
               , _command :: String }

-- parse dates
datep :: ReadM UTCTime
datep = eitherReader $ \arg -> case (cbParseDate arg) of
  Just t -> return t
  Nothing -> Left $ "cannot parse date `" ++ arg ++ "`; must be in YYYY/MM/DD format"

cmd :: Parser Cmd
cmd = Cmd
  <$> (option (liftA return datep)
       (long "date"
        <> short 'd'
        <> metavar "DATE"
        <> help ("If provided, Chorebot acts as if DATE were " ++
                 "the current date. Format as YYYY/MM/DD.")
        <> value getCurrentTime))
  <*> argument str ( metavar "COMMAND"
                     <> help "The command to run" )

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
  assigns <- do
    case assignmentsExists of
      True -> do
        assignmenttxt <- readFile assignmentfn
        case runAssignmentParser chores doers assignmentfn assignmenttxt of
          Right assigns' -> return assigns'
          Left err -> do
            putErr err
            exitFailure
      False -> return []

  let profiles = map (buildProfile assigns) doers

  (Cmd t' c) <- execParser (info (helper <*> cmd)
                           ( fullDesc
                           <> progDesc "chorebot"
                           <> header "chorebot -- blah blah"))
  t <- t' -- run the date IO action

  case c of

    "--help" ->
      putErr $ "usage: chorebot COMMAND\n\n" ++
               "commands:\n" ++
               "  list-chores              List current chores\n" ++
               "  list-doers               List current chore-doers\n" ++
               "  list-assignment-history  List past chore assignments\n" ++
               "  list-profiles            List profile info\n" ++
               "  distribute               Make new chore assignments"

    "list-chores" ->
      mapM_ (putStrLn . printChore) chores

    "list-doers" ->
      mapM_ (putStrLn . printDoer) doers

    "list-assignment-history" ->
      putStr $ printAssignments assigns

    "list-profiles" -> do
      putStrLn "name         diff/day  prev chores"
      putStrLn "----------------------------------"
      mapM_ (putStrLn . (printProfile t)) profiles

    "generate-doc" -> do
      readmetxt <- readFile "README.org"
      let doc = readOrg def readmetxt
          html = writeHtmlString def (handleError doc)
          prefix = "<!DOCTYPE html><head><title>Chorebot!</title>" ++
                   "<style>body { font-family: Helvetica, sans-serif; max-width: 600px; margin: 1em auto; }</style>" ++
                   "<body>"
          suffix = "</body></html>"
      putStrLn (prefix ++ html ++ suffix)

    "distribute" -> do
      gen <- getStdGen

      -- generate 1000 rounds of possible chore assignments
      let nIter = 1000
          (possibleAssignments, didForce, _gen) = foldl' iterDist ([],False,gen) (take nIter $ repeat ())
          iterDist (acc, dF, g) _ =
            let (newAssignments, dF', g') = distribute profiles chores assigns t g
            in (newAssignments:acc, dF || dF', g')

      case didForce of

        True -> do
          putErr "ERROR: sanity check flag flipped"

        False -> do

          let rankedPossibilities = zip (map (rank profiles) possibleAssignments) possibleAssignments

          case sortBy (\(x,_) (y,_) -> x `compare` y) rankedPossibilities of
            ((_rank, newAssigns):_rest) -> putStr $ printAssignments newAssigns
            _ -> return ()

    _ -> do
      putErr "unknown action (use --help for more information)"
      exitFailure
