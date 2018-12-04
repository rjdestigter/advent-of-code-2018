module One where

import           Data.List                      ( find )
import           Debug.Trace

dayOne :: IO (Int, Int)
dayOne = do
    linesOfFile <- readInputsFromFile
    let fns  = inputLinesToListOfFn linesOfFile
    let sum  = sumListOfFn fns
    let freq = findFrequency fns
    return (freq, sum)

plus :: Int -> Int -> Int
plus b a = a + b

minus :: Int -> Int -> Int
minus b a = a - b

-- Reads the input file provided by adventofcode.com
readInputsFromFile :: IO [String]
readInputsFromFile = do
    -- Read file contents
    inputFileContents <- readFile "./src/inputs/one.txt"
    -- Split into lines
    return (lines inputFileContents)

-- Converts each line into a partially applied function
-- So "+3" bcomes a function that takes a number and adds 3 to it
inputLine2Fn :: String -> (Int -> Int)
inputLine2Fn []       = plus 0
inputLine2Fn [x     ] = plus 0
inputLine2Fn (x : xs) = case x of
    '+' -> plus (read xs)
    '-' -> minus (read xs)
    _   -> plus 0

-- Converts a list of input lines to a list of partially applied functions
inputLinesToListOfFn :: [String] -> [Int -> Int]
inputLinesToListOfFn = fmap inputLine2Fn

-- Sums the list of of partially applied functions starting at applying zero
sumListOfFn :: [Int -> Int] -> Int
sumListOfFn = foldr (\a b -> a b) 0

-- Find the first frequency that appears twice
findFrequency :: [Int -> Int] -> Int
findFrequency operations = go operations [0] 0
  where
    go :: [Int -> Int] -> [Int] -> Int -> Int
    go [] frequencies accumulatedFrequency =
        go operations frequencies accumulatedFrequency
    go (nextOperation : tailOperations) frequencies accumulatedFrequency =
        let nextFrequency :: Int
            nextFrequency = nextOperation accumulatedFrequency

            twiceFrequency :: Maybe Int
            twiceFrequency = find (== nextFrequency) frequencies
        in  case twiceFrequency of
                Just _ -> nextFrequency
                _      -> go tailOperations
                             (frequencies ++ [nextFrequency])
                             nextFrequency
