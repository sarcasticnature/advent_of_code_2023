import System.Environment (getArgs)
import System.IO
import Data.Char

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ sum $ prune $ map numbers $ lines contents
    putStrLn "Part 2:"
    print $ sum $ map (pruneInt . readInts) $ lines contents
    --putStrLn "Debug:"
    --mapM_ print $ zip (lines contents) $ map (pruneInt . readInts) $ lines contents


numbers :: [Char] -> [Char]
numbers =
    let fn x acc = if isDigit x then x:acc else acc
    in  foldr fn ""

prune :: [String] -> [Int]
prune =
    let reader s = read $ head s : [last s]
        fn :: String -> [Int] -> [Int]
        fn s acc = reader s : acc
    in  foldr fn []

-- Note: only dropping 1 Char every time is inefficient, but catches cases like "sevenine"
-- speed isn't really a factor here, sooooo w/e
readInts :: String -> [Int]
readInts s
    | "" == s               = []
    | "1" == take 1 s       = 1 : readInts (drop 1 s)
    | "2" == take 1 s       = 2 : readInts (drop 1 s)
    | "3" == take 1 s       = 3 : readInts (drop 1 s)
    | "4" == take 1 s       = 4 : readInts (drop 1 s)
    | "5" == take 1 s       = 5 : readInts (drop 1 s)
    | "6" == take 1 s       = 6 : readInts (drop 1 s)
    | "7" == take 1 s       = 7 : readInts (drop 1 s)
    | "8" == take 1 s       = 8 : readInts (drop 1 s)
    | "9" == take 1 s       = 9 : readInts (drop 1 s)
    | "one" == take 3 s     = 1 : readInts (drop 1 s)
    | "two" == take 3 s     = 2 : readInts (drop 1 s)
    | "three" == take 5 s   = 3 : readInts (drop 1 s)
    | "four" == take 4 s    = 4 : readInts (drop 1 s)
    | "five" == take 4 s    = 5 : readInts (drop 1 s)
    | "six" == take 3 s     = 6 : readInts (drop 1 s)
    | "seven" == take 5 s   = 7 : readInts (drop 1 s)
    | "eight" == take 5 s   = 8 : readInts (drop 1 s)
    | "nine" == take 4 s    = 9 : readInts (drop 1 s)
    | otherwise             = readInts (drop 1 s)

pruneInt :: [Int] -> Int
pruneInt xs =
    let first = 10 * head xs
        second  = last xs
    in  first + second
