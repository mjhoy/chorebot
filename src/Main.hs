import System.Environment
import Chore

type Pattern = String

-- a doer of a choer
data Doer = Doer { name :: String,
                   vetoes :: [Pattern]
                 } deriving (Show, Eq)

main :: IO ()
main = do
  chores <- parseChores "chores.txt"
  case chores of
    (x:_) -> do
      putStrLn "First chore:"
      putStrLn $ show x
    _ -> do
      putStrLn "No chores!"
