module Chorebot.Doer where

import Data.Time

newtype Pattern = Pattern { unpattern :: String }
                  deriving (Show, Eq)

data Doer = Doer { name   :: String,
                   email  :: String,
                   vetoes :: [Pattern],
                   assign :: [Pattern],
                   absent :: [UTCTime]
                 } deriving (Eq, Show)

printDoer :: Doer -> String
printDoer d =
  "DOER: " ++ name d ++ " " ++
  "<" ++ email d ++ ">"
