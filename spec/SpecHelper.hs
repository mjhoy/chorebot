module SpecHelper
       ( module Test.Hspec
       , doersFixtureFile
       , fixtureData
       ) where

import Test.Hspec

import Chorebot.Doer
import Chorebot.Chore
import Chorebot.Assignment

import Chorebot.Doer.Parser
import Chorebot.Chore.Parser
import Chorebot.Assignment.Parser
import Paths_chorebot

doersFixtureFile, choresFixtureFile, assignmentsFixtureFile :: IO FilePath
doersFixtureFile = getDataFileName "spec/fixtures/doers.txt"
choresFixtureFile = getDataFileName "spec/fixtures/chores.txt"
assignmentsFixtureFile = getDataFileName "spec/fixtures/assignment-history.txt"

fixtureData :: IO ([Chore], [Doer], [Assignment])
fixtureData = do
  doersText <- doersFixtureFile >>= readFile
  let (Right doers) = runDoersParser "doers fixture" doersText

  choresText <- choresFixtureFile >>= readFile
  let (Right chores) = runChoresParser "chores fixture" choresText

  assignmentText <- assignmentsFixtureFile >>= readFile
  let (Right assignments) = runAssignmentParser chores doers "assignments fixture" assignmentText

  return (chores, doers, assignments)
