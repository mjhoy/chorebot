module Chorebot.DistributorSpec where

import SpecHelper
import Chorebot.Distributor
import Chorebot.Profile
import System.Random

spec :: Spec
spec = do

  describe "Chorebot.Distributor" $ do

    describe "distribute" $ do

      it "should make an assignment" $ do

        let (Just now) = cbParseDate "2016/01/15"
        (chores, (hildegard:_), assignments) <- fixtureData

        let prof = buildProfile assignments hildegard

        gen <- getStdGen

        -- first assignment. should assign Hildegard the chore of
        -- baking cookies.
        let (_newAssignments, sc, _) =
              distribute [prof] chores assignments now gen

        sc `shouldBe` True

      -- it "should make one assignment" $ do

      --   let (Just now) = cbParseDate "2016/01/15"
      --   (chores, (hildegard:_), assignments) <- fixtureData

      --   let prof = buildProfile assignments hildegard

      --   -- first assignment. should assign Hildegard the chore of
      --   -- baking cookies.
      --   let (chores', assignments', sc) =
      --         mkAssignment (chores, [], 0) prof 100 now

      --   (length chores') `shouldBe` ((length chores) - 1)
      --   (length assignments') `shouldBe` 1
      --   sc `shouldBe` 1
      --   (ident (chore (head assignments'))) `shouldBe` "bake-cookies"

      --   -- second assignment should *not* assign Hildegard the chore
      --   -- of mopping, since it is in her vetoes.
      --   let (chores'', assignments'', sc') =
      --         mkAssignment (chores', assignments', sc) prof 100 now

      --   (length chores'') `shouldBe` (length chores')
      --   (length assignments'') `shouldBe` 1
      --   sc' `shouldBe` 2        -- sc is incremented even though no
      --                           -- assignment made.

      --   -- third assignment should assign mopping, since we are
      --   -- setting the sanity check limit low.
      --   let (chores''', assignments''', sc'') =
      --         mkAssignment (chores'', assignments'', sc') prof 2 now

      --   (length chores''') `shouldBe` ((length chores'') - 1)
      --   (length assignments''') `shouldBe` 2
      --   sc'' `shouldBe` 3


    -- describe "assignPermanentChores" $ do

    --   it "should work on basic permanent assignments" $ do

    --     let (Just now) = cbParseDate "2016/01/15"
    --     (chores, doers, assignments) <- fixtureData

    --     (length chores) `shouldBe` 2

    --     let (chores', assignments') =
    --           assignPermanentChores (chores, assignments) doers now

    --     (length chores') `shouldBe` 1

main :: IO ()
main = hspec spec
