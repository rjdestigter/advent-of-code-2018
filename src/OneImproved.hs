-- | I wrote a first version using only singly linked lists.parse
-- I rewrote this with https://github.com/glguy/advent2018/blob/master/execs/Day01.hs solution
-- to learn some more function composition

module OneImproved where

import           Data.List                      ( find )
import qualified Data.Set                      as S

filename = "./src/inputs/one.txt"

dayOne :: IO ()
dayOne = do
    inputs <- readInputsFromFile
    print . sumList $ inputs
    print . findDuplicate . applyFrequency . parseList . cycle $ inputs
    return ()

sumList = sum . parseList

applyFrequency :: [Int] -> [Int]
applyFrequency = scanl (+) 0

-- | Reads the input file provided by adventofcode.com
readInputsFromFile :: IO [String]
readInputsFromFile = do
    inputFileContents <- readFile filename
    return . lines $ inputFileContents

parseList :: [String] -> [Int]
parseList = fmap parse

parse :: String -> Int
parse ('+' : xs) = read xs
parse ('-' : xs) = negate . read $ xs
parse _          = 0

findDuplicate :: [Int] -> Maybe Int
findDuplicate = go S.empty
  where
    go _ [] = Nothing
    go setOfFrequencies (x : xs)
        | S.member x setOfFrequencies = Just x
        | otherwise                   = go (S.insert x setOfFrequencies) xs
