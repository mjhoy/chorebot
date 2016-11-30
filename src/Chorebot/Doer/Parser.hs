module Chorebot.Doer.Parser (runDoersParser)
       where

import Text.Parsec
import Control.Monad
import Data.Char
import Data.Time.Clock (UTCTime)
import Data.List.Extra
import Data.Maybe (isJust)

import Chorebot.ParserHelper
import Chorebot.Time
import Chorebot.Doer

processKeys :: [(String, [String])] ->
               ([Pattern],           -- vetos
                [Pattern],           -- assigned
                [Maybe UTCTime],     -- absent
                Bool,                -- retired
                [(String, [String])] -- unknown key/values
               )
processKeys kvs = (_vet, _ass, _abs, _ret, _unknown)
  where
    kvs' = groupSort kvs
    kvs'' = for kvs' $ \(k,v) -> (k, concat v)
    flt k = filter ((== k) . fst) kvs''
    sub k = concatMap snd $ flt k
    subP k = for (sub k) $ \a -> Pattern a
    _vet = subP "veto"
    _ass = subP "assigned"
    _abs = map cbParseDate $ sub "absent"
    _ret = isJust $ find (\(k,_) -> k == "retired") kvs''
    _unknown = filter (\(k, _) ->
                        k /= "veto" &&
                        k /= "assigned" &&
                        k /= "absent" &&
                        k /= "retired") kvs''

doerParser :: Parsec String () Doer
doerParser = do
  skipMany commentOrNewline
  _name <- liftM trim $ many1 $ noneOf [ '<' ]
  _email <- emailParser
  _ <- newline
  kvs <- (keyvalue `endBy` newline) <?> "key values"
  let (_vet, _ass, _abs, _ret, _unknown) = processKeys kvs

  -- error on unknown keys
  forM_ _unknown $ \(k, _) ->
    unexpected ("key: " ++ k)

  -- error on bad time parsing
  _abs' <- forM _abs $ \v ->
    case v of
      Just t -> return t
      Nothing -> unexpected ("time format: please use YYYY/MM/DD")

  return $ Doer _name _email _vet _ass _abs' _ret

  where
    keyvalue = do
      key <- liftM (map toLower) $ many1 letter
      _ <- char ':'
      skipMany $ char ' '
      vals <- (many1 $ noneOf [ '\n', ',' ]) `sepBy` (char ',' >> (skipMany $ char ' '))
      return (key, vals)

doersParser :: Parsec String () [Doer]
doersParser = do
  doers <- doerParser `sepEndBy` (many1 (commentOrNewline <?> "comment sep"))
  return doers

runDoersParser :: String ->     -- filename to report errors with
                  String ->     -- text to parse
                  Either String [Doer]
runDoersParser filename str =
  case res of
    (Right doers) -> return doers
    (Left err)    -> Left $ show err
  where
    res = parse doersParser filename str
