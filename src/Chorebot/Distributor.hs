module Chorebot.Distributor where

import Data.Time
import Data.List

import Chorebot.Doer
import Chorebot.Chore
import Chorebot.Assignment

-- Calculate permanent assignments.
assignPermanentChores :: ([Chore],[Assignment]) -> -- chores to assign, current assignments
                         [Doer] ->                 -- doers to assign to
                         UTCTime ->                -- current date
                         ([Chore],[Assignment])    -- remaining chores, assignments + permanent assignments
assignPermanentChores ([],  assignments) _     _ = ([], assignments)
assignPermanentChores (rem, assignments) []    _ = (rem, assignments)
assignPermanentChores (rem, assignments) doers t = (rem', assignments')
  where
    (rem', assignments') = foldl' assignPermForDoer (rem, assignments) doers
    assignPermForDoer ([], as) _ = ([], as)
    assignPermForDoer (cs, as) d = (cs \\ assignedChores,
                                    as ++ (map (assign d t) assignedChores))
      where
        assignedChores   = foldl' assignPerm [] cs
        assignPerm acc c = if isPermanentlyAssigned d c
                           then c:acc
                           else acc

-- distribute :: [Profile] ->
--               [Chore] ->
--               UTCTime ->        -- current time
--               [Assignment]
-- distribute p c t = distribute' p c' a'
--   where
--     (c', a') = foldl' assignPermanents (c, []) profiles
--     assignPermanents (cs, as) p = foldl' assignPermanent (cs, as) cs
--       where
--         assignPermanent (cs', as') chore = if isPermanentlyAssigned (pdoer p) chore
--                                            then (delete chore cs', as' ++ chore)
--                                            else (cs', as')
--     distribute' [] _ a = a
--     distribute' _ [] a = a
--     distribute' profiles remaining assigned =
--       let (remaining'', assigned'') = foldl' assign (remaining, assigned) profiles
--           assign (r,a) p = case find (canAssign p) r of
--             Just chore -> (delete chore r, a ++ (assign chore (pdoer p) t))
--             Nothing    -> (r,a)
--       in distribute' profiles remaining' assigned'
--       where
--         distribute'' p = 
