module TimeCommon where

import Data.Time

cbParseDate :: String -> Maybe UTCTime
cbParseDate t = parseTimeM True defaultTimeLocale "%Y/%m/%d" t
