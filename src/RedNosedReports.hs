module RedNosedReports where

import Data.List (foldl')

type Level = Int
type Report = [Level]

parseIntoList :: String -> Report
parseIntoList = fmap read . words


parseInput :: String -> [Report]
parseInput =
    fmap parseIntoList
    . lines


type IsItSafe = Bool

type IsItSafeResult = Either String String

isItSafe :: Report -> IsItSafeResult
isItSafe = undefined
-- take two levels at a time and compare them


isPairSafe :: Int -> Int -> IsItSafeResult
isPairSafe a b =
    
    let difference = subtract a b
        absDifference = abs difference

        in if absDifference >= 1
            || absDifference <= 3

            then if difference > 0
                
                then Right $ (show [a, b]) ++ " are safe and increasing"

                else Right $ (show [a, b]) ++ " are safe and decreasing"

            else Left $ (show [a, b]) ++ " are UNSAFE"

