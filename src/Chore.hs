module Chore where

-- a chore to be done
data Chore = Chore { title :: String,
                     desc  :: String,
                     interval :: Int,
                     difficulty :: Int
                   } deriving (Show, Eq)

parseChores :: String -> IO [Chore]
parseChores _ = return []
