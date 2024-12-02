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
                "[[7,6,4,2,1],[1,2,7,8,9],[9,7,6,2,1],[1,3,2,4,5],[8,6,4,4,1],[1,3,6,7,9]]"

    describe "isPairSafe 1 2" $ do
        it "increasing"
            $ shouldBe
                (show $ isPairSafe 1 2)
                "Right \"[1,2] are safe and increasing\""
