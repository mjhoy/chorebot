module Chorebot.Doer.Parser (runDoersParser)
       where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad
import Data.Char
import Data.List.Extra (trim)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.List.Extra

import Chorebot.ParserHelper
import Chorebot.Time
import Chorebot.Doer

processKeys :: [(String, [String])] ->
               ([Pattern],           -- vetos
                [Pattern],           -- assigned
                [Maybe UTCTime],     -- absent
                [(String, [String])] -- unknown key/values
               )
processKeys kvs = (_vet, _ass, _abs, _unknown)
  where
    kvs' = groupSort kvs
    kvs'' = for kvs' $ \(k,v) -> (k, concat v)
    flt k = filter ((== k) . fst) kvs''
    sub k = concatMap snd $ flt k
    subP k = for (sub k) $ \a -> Pattern a
    _vet = subP "veto"
    _ass = subP "assigned"
    _abs = map cbParseDate $ sub "absent"
    _unknown = filter (\(k, v) ->
                        k /= "veto" &&
                        k /= "assigned" &&
                        k /= "absent") kvs''

doerParser :: Parsec String () Doer
doerParser = do
  skipMany commentOrNewline
  _name <- liftM trim $ many1 $ noneOf [ '<' ]
  _email <- emailParser
  newline
  kvs <- (keyvalue `endBy` newline) <?> "key values"
  let (_vet, _ass, _abs, _unknown) = processKeys kvs

  -- error on unknown keys
  forM_ _unknown $ \(k, v) ->
    unexpected ("key: " ++ k)

  -- error on bad time parsing
  _abs' <- forM _abs $ \v ->
    case v of
      Just t -> return t
      Nothing -> unexpected ("time format: please use YYYY/MM/DD")

  return $ Doer _name _email _vet _ass _abs'

  where
    keyvalue = do
      key <- liftM (map toLower) $ many1 letter
      _ <- char ':'
      vals <- (many1 $ noneOf [ '\n', ',' ]) `sepBy` (char ',')
      return (key, vals)

doersParser :: Parsec String () [Doer]
doersParser = do
  doers <- doerParser `sepEndBy` (many1 (commentOrNewline <?> "comment sep"))
  return doers

runDoersParser :: String -> String -> Either String [Doer]
runDoersParser filename str =
  case res of
    (Right doers) -> return doers
    (Left err)    -> Left $ show err
  where
    res = parse doersParser filename str
