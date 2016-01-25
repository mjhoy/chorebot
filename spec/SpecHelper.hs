module SpecHelper
       ( module Test.Hspec
       , doersFixtureFile
       ) where

import Test.Hspec
import Paths_chorebot

doersFixtureFile :: IO FilePath
doersFixtureFile = getDataFileName "spec/fixtures/doers.txt"
