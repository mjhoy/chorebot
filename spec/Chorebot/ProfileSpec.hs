module Chorebot.ProfileSpec where

import SpecHelper

import Chorebot.Profile
import Chorebot.Time

spec :: Spec
spec = do

  describe "difficultyPerDay" $ do
    it "should give us the difficulty over the number of days" $ do

      let (Just now) = cbParseDate "2016/01/15"
      (chores, doers, assignments) <- fixtureData

      let (hildegardProfile:_) = map (buildProfile assignments) doers

      (difficultyPerDay now hildegardProfile) `shouldBe` ((7 + 4) / 14)

      let (Just now') = cbParseDate "2016/01/17"
      (difficultyPerDay now' hildegardProfile) `shouldBe` ((7 + 4) / 16)

main :: IO ()
main = hspec spec
