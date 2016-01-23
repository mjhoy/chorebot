module Chorebot.Profile where

import Chorebot.Doer
import Chorebot.Chore
import Chorebot.Assignment

import Data.List

-- A profile is simply a doer associated with their assigned chore
-- history. And from this we can extrapolate interesting information.
data Profile = Profile { pdoer :: Doer,
                         assignments :: [Assignment]
                       } deriving (Eq, Show)

buildProfile :: Doer ->
                [Assignment] -> -- List of all/any chore assignments
                Profile
buildProfile doer' assignments = Profile doer' assignments''
  where
    assignments'' = sortBy (\a b -> (date a) `compare` (date b)) assignments'
    assignments' = filter byDoer assignments
    byDoer a = (doer a) == doer'
