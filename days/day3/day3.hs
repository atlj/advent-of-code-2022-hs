import Data.Char (isUpperCase)
import Data.List.Split (splitEvery)
import Data.Set (fromList, toList)

puzzleInput :: IO String
puzzleInput = readFile "/Users/burakguner/kod/advent-of-code-2022-hs/days/day3/input.txt"

splitHalf :: [a] -> ([a], [a])
splitHalf list = splitAt ((length list + 1) `div` 2) list

findPairs :: (Ord a) => [a] -> [a] -> [a]
findPairs firstList secondList = removeDuplicates [x | x <- firstList, x `elem` secondList]

findCommon :: (Eq a) => [a] -> [a] -> [a] -> a
findCommon (x : xs) second third
  | x `elem` second && x `elem` third = x
  | otherwise = findCommon xs second third

calculateGroup :: [String] -> Int
calculateGroup [first, second, third] = calculatePoint $ findCommon first second third

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

-- A = 27
-- Unicode A = 65
uppercaseOffset :: Int
uppercaseOffset = 38 -- 65 - 27

-- a = 1
-- Unicode a = 97
lowercaseOffset :: Int
lowercaseOffset = 96 -- 97 - 1

calculatePoint :: Char -> Int
calculatePoint char =
  let offset = if isUpperCase char then uppercaseOffset else lowercaseOffset
   in fromEnum char - offset

calculateLine :: String -> Int
calculateLine line = sum . map calculatePoint $ uncurry findPairs $ splitHalf line

main :: IO ()
main = do
  input <- puzzleInput
  print . sum . map calculateLine $ lines input
  print . sum . map calculateGroup $ splitEvery 3 $ lines input
