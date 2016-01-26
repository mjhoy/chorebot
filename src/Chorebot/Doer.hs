module Chorebot.Doer where

import Data.Time
import Text.Regex (mkRegex, Regex)

newtype Pattern = Pattern { unpattern :: String }
                  deriving (Show, Eq)

data Doer = Doer { name   :: String,
                   email  :: String,
                   vetoes :: [Pattern],
                   perm   :: [Pattern],
                   absent :: [UTCTime]
                 } deriving (Eq, Show)

patToRegex :: Pattern -> Regex
patToRegex (Pattern str) = mkRegex str

printDoer :: Doer -> String
printDoer d =
  name d ++ " " ++
  "<" ++ email d ++ ">"
