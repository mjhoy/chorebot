module Chorebot.Assignment.Parser
       (runAssignmentParser)
       where

import Text.Parsec
import Text.Parsec.Char
import Data.Time
import Data.Either
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map (Map)

import Chorebot.ParserHelper
import Chorebot.Time
import Chorebot.Assignment
import Chorebot.Chore
import Chorebot.Doer

-- assignment before it has been associated with an actual chore or
-- doer
data ProtoAssignment =
  ProtoAssignment { _p_chore :: String, -- Chore ident
                    _p_doer  :: String, -- Doer email
                    _p_adiff :: Int
                  } deriving (Show, Eq)

assignmentParser :: Parsec String () [ProtoAssignment]
assignmentParser = do
  skipMany commentOrNewline
  -- doer name: skip
  skipMany1 $ noneOf [ '<' ]
  _email <- emailParser
  _ <- char ':'
  skipMany $ char ' '
  newline
  as <- manyTill (assignmentParser' _email) $
    (lookAhead (string "\n") >> return ()) <|> eof
  skipMany commentOrNewline
  return as
  where
    assignmentParser' _email = do
      skipMany $ noneOf [ '<' ]
      _chore <- identParser
      skipMany $ char ' '
      _diff <- liftM read $ (many1 digit <?> "difficulty")
      newline
      return $ ProtoAssignment _chore _email _diff

dateAndAssignmentsParser :: Parsec String () (UTCTime, [ProtoAssignment])
dateAndAssignmentsParser = do
  skipMany commentOrNewline
  _ <- char '['
  str <- many1 (digit <|> char '/')
  _ <- char ']'
  case (cbParseDate str) of
    Nothing -> unexpected $ "time format: " ++ str
    Just t -> do
      pAssignments <- manyTill assignmentParser ((lookAhead (char '[') >> return ()) <|> eof)
      return $ (t, concat pAssignments)

assignmentsParser :: [Chore] ->
                     [Doer]  ->
                     Bool ->    -- strict?
                     Parsec String () [Assignment]
assignmentsParser chores doers strict = do
  pAssignments <- many dateAndAssignmentsParser
  eof
  let pAssignments' = concatMap resolve pAssignments
  case strict of
    True  -> do
      forM_ pAssignments' strictCheck
      return $ rights pAssignments'
    False ->
      return $ rights pAssignments'
  where
    resolve (t, as) = map (resolveAssignment chores doers t) as
    strictCheck (Right _)  = return ()
    strictCheck (Left err) = unexpected err

-- Given a list of chores and doers, resolve a proto-assignment (with
-- string identifier references) into a Right Assignment, or a Left
-- err, with err specifying if a chore or doer is missing.
resolveAssignment :: [Chore] ->
                     [Doer]  ->
                     UTCTime ->
                     ProtoAssignment ->
                     Either String Assignment
resolveAssignment chores doers atime proto = do
  let cmap :: Map String Chore
      cmap = Map.fromList $ map (\c -> (ident c, c)) chores
      dmap :: Map String Doer
      dmap = Map.fromList $ map (\d -> (email d, d)) doers
  chore <- case Map.lookup (_p_chore proto) cmap of
    Just x  -> return x
    Nothing -> Left $ "chore not found: <" ++ (_p_chore proto) ++ ">"
  doer  <- case Map.lookup (_p_doer proto) dmap of
    Just x  -> return x
    Nothing -> Left $ "doer not found: <" ++ (_p_doer proto) ++ ">"
  return $ Assignment chore doer atime (_p_adiff proto)

runAssignmentParser :: [Chore] -> -- list of parsed chores
                       [Doer]  ->  -- list of parsed doers
                       String  ->
                       String  ->
                       Either String [Assignment]
runAssignmentParser chores doers filename str =
  case res of
    (Right assignments) -> return assignments
    (Left err)          -> Left $ show err
  where
    res = parse (assignmentsParser chores doers True) filename str
