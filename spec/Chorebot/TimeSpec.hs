module Chorebot.TimeSpec where

import SpecHelper
import Chorebot.Time

spec :: Spec
spec = do
  describe "Chorebot.Time" $ do
    describe "cbParseDate" $ do
      it "returns nothing for a bad date" $ do
        let res = cbParseDate "12/22"
        res `shouldBe` Nothing
