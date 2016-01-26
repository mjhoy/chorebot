module Chorebot.DistributorSpec where

import SpecHelper
import Chorebot.Distributor

spec :: Spec
spec = do

  describe "Chorebot.Distributor" $ do

    describe "assignPermanentChores" $ do

      it "should work on basic permanent assignments" $ do

        let (Just now) = cbParseDate "2016/01/15"
        (chores, doers, assignments) <- fixtureData

        (length chores) `shouldBe` 2

        let (chores', assignments') =
              assignPermanentChores (chores, assignments) doers now

        (length chores') `shouldBe` 1

main :: IO ()
main = hspec spec
