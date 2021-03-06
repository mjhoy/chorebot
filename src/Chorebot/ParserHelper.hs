module Chorebot.ParserHelper where

import Text.Parsec
import qualified Data.Set as Set

identParser :: Parsec String () String
identParser = do
  _ <- char '<'
  r <- many1 (alphaNum <|> char '-')
  _ <- char '>'
  return r <?> "identifier"

emailParser :: Parsec String () String
emailParser = do
  _   <- char '<' <?> "email opening bracket"
  ret <- (many1 $ noneOf [ '>' ]) <?> "email address"
  _   <- char '>' <?> "email closing bracket"
  return ret <?> "email address"

comment :: Parsec String () Char
comment = do
  _ <- char '#'
  skipMany $ noneOf [ '\n' ]
  newline

commentOrNewline :: Parsec String () Char
commentOrNewline = comment <|> newline

-- Returns just the first duplicate in list, or Nothing if no
-- duplicates are found.
findDup :: Ord a => [a] -> Maybe a
findDup ls = findDup' ls Set.empty
  where findDup' [] _ = Nothing
        findDup' (x:xs) s =
          case Set.member x s of
            True -> Just x
            False -> findDup' xs (Set.insert x s)
