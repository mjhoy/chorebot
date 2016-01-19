module DoerParser (runDoersParser)
       where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad
import Data.Char
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import ParserCommon
import Doer
import Data.List.Extra

doerParser :: Parsec String () Doer
doerParser = do
  skipMany commentOrNewline
  _name <- many1 $ noneOf [ '<' ]
  _email <- do
    char '<' <?> "email opening bracket"
    ret <- (many1 $ noneOf [ '>' ]) <?> "email address"
    char '>' <?> "email closing bracket"
    return ret <?> "email address"
  newline
  kvs <- (keyvalue `endBy` newline) <?> "key values"
  let kvs' = groupSort kvs
      kvs'' = for kvs' $ \(k,v) -> (k, concat v)
      flt k = filter ((== k) . fst) kvs''
      sub k = concatMap snd $ flt k
      subP k = for (sub k) $ \a -> Pattern a
      parseTime' :: String -> Maybe UTCTime
      parseTime' t = parseTimeM True defaultTimeLocale "%Y/%m/%d" t
      _vet = subP "veto"
      _ass = subP "assigned"
      _abs = map parseTime' $ sub "absent"
      _unknown = filter (\(k, v) ->
                          k /= "veto" &&
                          k /= "assigned" &&
                          k /= "absent") kvs''
  forM_ _unknown $ \(k, v) ->
    unexpected ("value: " ++ k)
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
