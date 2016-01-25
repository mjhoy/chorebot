module Chorebot.Doer.ParserSpec where

import SpecHelper
import Chorebot.Doer.Parser
import Chorebot.Doer
import Paths_chorebot

spec :: Spec
spec = do
  describe "Chorebot.Doer.Parser" $ do

    describe "runDoersParser" $ do

      -- see spec/fixtures/doers.txt

      it "should correctly parse hildegard" $ do

        doersText <- doersFixtureFile >>= readFile
        let (Right doers) = runDoersParser "doers fixture" doersText

        let (hildegard:_) = doers

        name  hildegard `shouldBe` "St. Hildegard"
        email hildegard `shouldBe` "hildegard@test.com"
        let (v1:v2:v3:_) = vetoes hildegard
        v1 `shouldBe` (Pattern "mop*")
        v2 `shouldBe` (Pattern "clean-room")
        v3 `shouldBe` (Pattern "extra-space")

main :: IO ()
main = hspec spec
