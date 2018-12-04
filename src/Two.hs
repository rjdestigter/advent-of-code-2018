-- | I wrote a first version using only singly linked lists.parse
-- I rewrote this with https://github.com/glguy/advent2018/blob/master/execs/Day01.hs solution
-- to learn some more function composition

module Two where

import           Data.List                      ( find )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map

filename = "./src/inputs/two.txt"

dayTwo :: IO ()
dayTwo = do
    inputs <- readInputsFromFile
    return ()

    -- Reads the input file provided by adventofcode.com
readInputsFromFile :: IO ()
readInputsFromFile = do
    inputFileContents <- readFile filename
    print
        . uncurry (*)
        . foldr plus (0, 0)
        . fmap charmap2count
        . strings2Maps
        . lines
        $ inputFileContents
    print
        -- . filter (\(_, _, i) -> i == 1)
        . compareStrings
        . lines
        $ inputFileContents
    return ()


type CharMap = Map.Map Char Int

strings2Maps :: [String] -> [CharMap]
strings2Maps = fmap string2Map

string2Map :: String -> CharMap
string2Map = foldr foldChar Map.empty
  where
    foldChar :: Char -> CharMap -> CharMap
    foldChar char charmap =
        let entry  = Map.lookup char charmap
            insert = flip (Map.insert char) charmap
        in  case entry of
                Just n -> insert $ 1 + n
                _      -> insert 1

type Twos = Int
type Threes = Int

charmap2count :: CharMap -> (Twos, Threes)
charmap2count = Map.foldr foldCharmap (0, 0)
  where
    foldCharmap :: Int -> (Twos, Threes) -> (Twos, Threes)
    foldCharmap count (twos, threes) = case (twos, threes) of
        (1, 1) -> (1, 1)
        _      -> case count of
            2 -> (1, threes)
            3 -> (twos, 1)
            _ -> (twos, threes)

plus :: (Twos, Threes) -> (Twos, Threes) -> (Twos, Threes)
plus (a, b) (c, d) = (a + c, b + d)

compareStrings :: [String] -> [(String, String, Int)]
compareStrings inputs =
    let mapTo inp1 inp2 = (inp1, inp2, stringCompare inp1 inp2)
        ftr (_, _, i) = i == 1

        go inp = filter ftr $ fmap (mapTo inp) inputs
    in  (>>=) inputs go

stringCompare :: String -> String -> Int
stringCompare = go 0
  where
    go i []       []       = i
    go i []       b        = go (i + 1) [] b
    go i a        []       = go (i + 1) a []
    go i (a : as) (b : bs) = go (if a == b then i else i + 1) as bs
