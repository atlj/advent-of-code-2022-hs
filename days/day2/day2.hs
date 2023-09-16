import Data.Maybe (mapMaybe)

puzzleInput :: IO String
puzzleInput = readFile "/Users/burakguner/kod/practice-hs/days/day2/input.txt"

data Hand = Rock | Paper | Scissors deriving (Eq, Show)

handFromPick :: Char -> Maybe Hand
handFromPick pick = case pick of
  'A' -> Just Rock
  'X' -> Just Rock
  'B' -> Just Paper
  'Y' -> Just Paper
  'C' -> Just Scissors
  'Z' -> Just Scissors
  _ -> Nothing

outcomePointsFromPicks :: Hand -> Hand -> Int
outcomePointsFromPicks yourPick opponentPick
  | yourPick == opponentPick = 3
  | otherwise = case (yourPick, opponentPick) of
      (Rock, Scissors) -> 6
      (Paper, Rock) -> 6
      (Scissors, Paper) -> 6
      _ -> 0

pointsFromPick :: Hand -> Int
pointsFromPick hand = case hand of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

calculate :: Hand -> Hand -> Maybe Int
calculate yourPick opponentPick = do
  Just $ pointsFromPick yourPick + outcomePointsFromPicks yourPick opponentPick

extractPicks :: String -> Maybe (Hand, Hand)
extractPicks line = case line of
  [opponentPickChar, ' ', yourPickChar] -> do
    yourPick <- handFromPick yourPickChar
    opponentPick <- handFromPick opponentPickChar
    Just (yourPick, opponentPick)
  _ -> Nothing

calculateLineOutcome :: String -> Maybe Int
calculateLineOutcome line = do
  picks <- extractPicks line
  uncurry calculate picks

main :: IO ()
main = do
  input <- puzzleInput
  print . sum . mapMaybe calculateLineOutcome $ lines input
