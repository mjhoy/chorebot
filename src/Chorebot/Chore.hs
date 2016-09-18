module Chorebot.Chore where

-- a chore to be done
data Chore = Chore { choreTitle :: String
                   , choreIdent :: String -- unique identifier
                   , choreDesc  :: String
                   , choreInterval :: Int
                   , choreDifficulty :: Int
                   , choreCount :: Int
                   } deriving (Show, Eq)

printChore :: Chore -> String
printChore chore =
  let counts = if (choreCount chore /= 1)
               then " " ++ (show (choreCount chore)) ++ "x "
               else " "
      c = (choreTitle chore) ++ " <" ++ (choreIdent chore) ++ "> " ++
          ": " ++ (show $ choreInterval chore) ++ counts ++
          (show $ choreDifficulty chore)
  in case length (choreDesc chore) of
    0 -> c ++ "\n"
    _ -> c ++ "\n" ++ (choreDesc chore) ++ "\n"
