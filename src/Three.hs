-- | I wrote a first version using only singly linked lists.parse
-- I rewrote this with https://github.com/glguy/advent2018/blob/master/execs/Day01.hs solution
-- to learn some more function composition

module Three where

import           Data.List
import           Data.List.Split                ( splitOn )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map

filename = "./src/inputs/three.txt"

type Claim = (Int, Int, Int, Int, Int)

dayThree :: IO ()
dayThree = do
    inputs <- readInputsFromFile
    print . semiSplit $ inputs
    return ()

semiSplit = fmap (toClaim . splitOn ";")

type Result = (Set.Set (Int, Int), Set.Set (Int, Int))

foldClaims :: [Claim] -> Result
foldClaims = foldr foldClaim (Set.empty, Set.empty)
  where
    foldClaim :: Claim -> Result -> Result
    foldClaim (id, x, y, w, h) (all, overlaps) = (all, overlaps)

toClaim :: [String] -> Maybe Claim
toClaim (a : b : c : d : e : xs) =
    Just (read a, read b, read c, read d, read e)
toClaim _ = Nothing

-- | Reads the input file provided by adventofcode.com
readInputsFromFile :: IO [String]
readInputsFromFile = do
    inputFileContents <- readFile filename
    return . lines $ inputFileContents
