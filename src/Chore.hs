module Chore where

-- a chore to be done
data Chore = Chore { title :: String,
                     desc  :: String,
                     interval :: Int,
                     difficulty :: Int
                   } deriving (Show, Eq)

printChore :: Chore -> String
printChore chore =
  let c = (title chore) ++ ": " ++ (show $ interval chore) ++ " " ++
          (show $ difficulty chore)
  in case length (desc chore) of
    0 -> c ++ "\n"
    _ -> c ++ "\n" ++ (desc chore) ++ "\n"
