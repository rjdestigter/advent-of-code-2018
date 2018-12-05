-- | I wrote a first version using only singly linked lists.parse
-- I rewrote this with https://github.com/glguy/advent2018/blob/master/execs/Day01.hs solution
-- to learn some more function composition

module Three where

import           Data.List
import           Data.List.Split                ( splitOn )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map

import Data.Maybe (fromJust)

filename = "./src/inputs/three.txt"

type Claim = (Int, Int, Int, Int, Int)

dayThree :: IO ()
dayThree = do
    inputs <- readInputsFromFile
    let (all, overlap, allIds, overlapIds) = foldClaims . semiSplit $ inputs
    print . Set.size $ overlap
    print $  Set.difference allIds overlapIds
    return ()

semiSplit = fmap (fromJust . toClaim . splitOn ";")
 
type Result = (Set.Set (Int, Int), Set.Set (Int, Int), Set.Set Int, Set.Set Int)

claims :: [Claim]
claims = [ (1, 0, 0, 3, 3)
         , (2, 1, 1, 2, 2 )
         ]

foldClaims :: [Claim] -> Result
foldClaims = foldr foldClaim (Set.empty, Set.empty, Set.empty, Set.empty)
  where 
    foldClaim :: Claim -> Result -> Result
    foldClaim (id, x, y, w, h) results = 
        let
            rangeX = [x..x + w - 1]
            in
                foldr foldX results rangeX
                where
                    rangeY = [y..y + h - 1]
                    foldX :: Int -> Result -> Result
                    foldX rx result =
                        foldr foldY result rangeY
                            where

                                foldY :: Int -> Result -> Result
                                foldY ry (all, overlaps, allIds, overlapIds) = 
                                    if Set.member (rx, ry) all then
                                        (all, Set.insert (rx, ry) overlaps, Set.insert id allIds, Set.insert id overlapIds)
                                    else
                                        (Set.insert (rx, ry) all, overlaps, Set.insert id allIds, overlapIds)
                                    

toClaim :: [String] -> Maybe Claim
toClaim (a : b : c : d : e : xs) =
    Just (read a, read b, read c, read d, read e)
toClaim _ = Nothing

-- | Reads the input file provided by adventofcode.com
readInputsFromFile :: IO [String]
readInputsFromFile = do
    inputFileContents <- readFile filename
    return . lines $ inputFileContents
