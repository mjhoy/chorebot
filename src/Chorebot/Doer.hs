module Chorebot.Doer where

import Data.Time
import Text.Regex (mkRegex, Regex)

newtype Pattern = Pattern { unpattern :: String }
                  deriving (Show, Eq)

data Doer = Doer { doerName   :: String,
                   doerEmail  :: String,
                   doerVetoes :: [Pattern],
                   doerPerm   :: [Pattern],
                   doerAbsent :: [UTCTime]
                 } deriving (Eq, Show)

instance Ord Doer where
  d1 `compare` d2 = (doerEmail d1) `compare` (doerEmail d2)

patToRegex :: Pattern -> Regex
patToRegex (Pattern str) = mkRegex str

printDoer :: Doer -> String
printDoer d =
  doerName d ++ " " ++
  "<" ++ doerEmail d ++ ">"
