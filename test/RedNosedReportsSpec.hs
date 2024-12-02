module RedNosedReportsSpec where 

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import RedNosedReports

spec :: Spec
spec = do
    testInput <- runIO $ readFile "testInput"

    --input <- runIO $ readFile "input.txt"

    describe "parseInput" $ do
        it "works"
            $ shouldBe
                (show $ parseInput testInput)
                "42"

    describe "isPairSafe" $ do
        it "works"
            $ shouldBe
                (show $ isPairSafe 1 1)
                "42"
