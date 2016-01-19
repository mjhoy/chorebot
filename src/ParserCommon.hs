module ParserCommon where

import Text.Parsec
import Text.Parsec.Error

comment :: Parsec String () Char
comment = do
  _ <- char '#'
  skipMany $ noneOf [ '\n' ]
  newline

commentOrNewline :: Parsec String () Char
commentOrNewline = comment <|> newline
