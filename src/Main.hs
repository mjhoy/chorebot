import System.Environment
import System.IO
import ChoreParser

type Pattern = String

-- a doer of a choer
data Doer = Doer { name :: String,
                   vetoes :: [Pattern]
                 } deriving (Show, Eq)

main :: IO ()
main = do
  let choresfn = "chores.txt"
  chorestxt <- readFile choresfn
  case runChoresParser choresfn chorestxt of
    Right chores -> do
      mapM_ (putStrLn . show) chores
    Left err -> do
      hPutStrLn stderr err
