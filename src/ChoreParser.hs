module ChoreParser (runChoresParser)
       where

import Text.Parsec
import Text.Parsec.Error
import ParserCommon
import Chore

titleParser :: Parsec String () String
titleParser = many1 $ noneOf [ '\n', ':' ]

difficultyParser :: Parsec String () Int
difficultyParser = do
  str <- try (string "Hard")
     <|> try (string "Medium")
     <|> try (string "Easy")
  case str of
    "Hard"   -> return 7
    "Medium" -> return 4
    "Easy"   -> return 2
    _ -> unexpected "difficulty -- unknown"

choreParser :: Parsec String () Chore
choreParser = do
    skipMany commentOrNewline
    _title <- titleParser
    _ <- char ':'
    spaces'
    _intervalStr <- many1 digit
    skipMany1 space'
    _difficulty <- difficultyParser
    spaces'
    _ <- newline
    _ident <- identParser
    _ <- newline
    _desc <- (newline >> return "")
         <|> manyTill anyChar (try (blankSpace <|> eofAndSpaces))
    return $ Chore _title _ident _desc (read _intervalStr) _difficulty
  where
    blankSpace = string "\n\n" >> return ()
    eofAndSpaces = skipMany space >> eof
    space' = char ' '
    spaces' = skipMany space'

choresParser :: Parsec String () [Chore]
choresParser = do
  unchore
  chores <- choreParser `endBy` unchore
  -- ensure identifies are unique
  case findDup (map ident chores) of
    Just x  -> unexpected $ "duplicate chore identified: " ++ x
    Nothing -> return ()
  return chores
  where
    unchore = skipMany commentOrNewline >> return ()

runChoresParser :: String -> String -> Either String [Chore]
runChoresParser filename str =
  case res of
    (Right cs) -> return cs
    (Left err) -> Left $ show err
  where
    res = parse choresParser filename str
