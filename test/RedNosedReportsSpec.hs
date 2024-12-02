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
                (parseInput testInput)
                "42"

