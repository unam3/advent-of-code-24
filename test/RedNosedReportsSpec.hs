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

    describe "makeTupleList [1,2,3,4]" $ do
        it "works"
            $ shouldBe
                (show $ makeTupleList [1,2,3,4])
                "[(1,2),(2,3),(3,4)]"

    describe "isPairSafe" $ do
        it "1 2"
            $ shouldBe
                (show $ isPairSafe Nothing 1 2)
                "Right (\"[1,2] are safe and increasing\",Just True)"

        it "2 1"
            $ shouldBe
                (show $ isPairSafe Nothing 2 1)
                "Right (\"[2,1] are safe and decreasing\",Just False)"

        it "(Just True) 2 1"
            $ shouldBe
                (show $ isPairSafe (Just True) 2 1)
                "Left \"[2,1] are UNSAFE: inconsistent direction\""

        it "6 2"
            $ shouldBe
                (show $ isPairSafe Nothing 6 2)
                "Left \"[6,2] are UNSAFE\""


    --describe "isReportSafe" $ do
    --    it "works with testInput"
    --        $ shouldBe
    --            (show $ fmap isReportSafe $ parseInput testInput)
    --            "Right \"[1,2] are safe and increasing\""
