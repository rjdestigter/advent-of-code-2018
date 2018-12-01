module One where

run :: IO Int
run = do
    linesOfFile <- readInputsFromFile
    let fns = inputLinesToListOfFn linesOfFile
    return $ sumListOfFn fns

plus :: Int -> Int -> Int
plus b a = a + b

minus :: Int -> Int -> Int
minus b a = a - b

readInputsFromFile :: IO [String]
readInputsFromFile = do
    inputFileContents <- readFile "./assets/one.txt"
    let linesOfFile = lines inputFileContents
    return linesOfFile

inputLine2Fn :: String -> (Int -> Int)
inputLine2Fn [] = plus 0
inputLine2Fn (x:[]) = plus 0
inputLine2Fn (x:xs) = case x of
    '+' -> plus (read xs)
    '-' -> minus (read xs)
    _ -> plus 0

inputLinesToListOfFn :: [String] -> [Int -> Int]
inputLinesToListOfFn = fmap inputLine2Fn

sumListOfFn :: [Int -> Int] -> Int
sumListOfFn = foldr (\a b -> a b) 0

