import Data.List
import Data.List.Split (splitOn)
import Data.Ord

input :: IO String
input = readFile "/Users/burakguner/kod/practice-hs/days/day1/input.txt"

elves :: String -> [String]
elves input = splitOn "\n\n" input

calculateElfCallories :: String -> [Int]
calculateElfCallories elf = map (\x -> read x :: Int) $ lines elf

main :: IO ()
main = do
  contents <- input
  print . sum . take 3 . sortBy (comparing Data.Ord.Down) . map (sum . calculateElfCallories) $ elves contents
