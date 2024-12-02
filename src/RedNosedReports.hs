module RedNosedReports where

--import Data.List (foldl')

type Level = Int
type Report = [Level]

parseIntoList :: String -> Report
parseIntoList = fmap read . words


parseInput :: String -> [Report]
parseInput =
    fmap parseIntoList
    . lines


type IsItSafeResult = Either String (String, MaybeDirection)

--isIncreasing = True
--
--isDecreasing = False

type IsDirection = Bool

type MaybeDirection = Maybe IsDirection

isPairSafe :: MaybeDirection -> Int -> Int -> IsItSafeResult
isPairSafe maybeDirection a b =
    
    let difference = subtract a b
        absDifference = abs difference

        in if absDifference >= 1
            && absDifference <= 3

            then if difference > 0
                && (maybeDirection == Nothing
                    || (maybeDirection == (Just True))
                    )

                    then Right (
                        
                        (show [a, b]) ++ " are safe and increasing",
                        
                        Just True
                        )

                    else if
                        maybeDirection == Nothing
                        || (maybeDirection == (Just False)
                        )
                        
                        then Right (
                            (show [a, b]) ++ " are safe and decreasing",
                            Just False
                            )
                        else Left $ (show [a, b]) ++ " are UNSAFE: inconsistent direction"

            else Left $ (show [a, b]) ++ " are UNSAFE"



makeTupleList :: [a] -> [(a, a)]

makeTupleList list = zip list $ tail list


isReportSafe :: Report -> [IsItSafeResult]
isReportSafe =
    
    fmap (\ (a, b) -> isPairSafe Nothing a b)
        
    . makeTupleList
