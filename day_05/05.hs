import System.Environment (getArgs)
import System.IO
import Data.Char (isAlpha, isSpace)
import Data.List (nub, foldl')
import Control.Parallel.Strategies

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    let seeds = parseSeeds $ lines contents
    let maps = parseMaps $ chunks $ drop 2 $ lines contents
    print $ minimum $ map (runSeed maps) seeds
    putStrLn "Part 2:"
    --print maps
    --print $ parseRanges $ lines contents
    print $ runRanges maps $ parseRanges $ lines contents

-- data

type Mapping = (Int, Int, Int)
type Range = (Int, Int)

-- part 1

parseSeeds :: [String] -> [Int]
parseSeeds = map read . tail . words . head

chunks :: [String] -> [String]
chunks [] = []
chunks (x:xs)
    | x == ""           = "" : chunks xs
    | x == "\n"         = chunks xs
    | isAlpha $ head x  = chunks xs
    | otherwise         = x : chunks xs

toMapping :: String -> Mapping
toMapping s =
    let s' = map read $ words s
        end = head s'
        begin = s' !! 1
        span = last s'
    in  (begin, begin + (span - 1), end - begin)

parseMaps :: [String] -> [[Mapping]]
parseMaps xs =
    let f x (acc, tmp) = if x == "" then (tmp : acc, []) else (acc, toMapping x : tmp)
        (most, rest) = foldr f ([], []) xs
    in  rest:most

begin :: Mapping -> Int
begin (b, _, _) = b

end :: Mapping -> Int
end (_, e, _) = e

offset :: Mapping -> Int
offset (_, _, r) = r

runMap :: Int -> [Mapping] -> Int
runMap n ms =
    let f m (found, acc) = if not found && begin m <= n && end m >= n
                           then (True, n + offset m)
                           else (False, acc)
    in  snd $ foldr f (False, n) ms

runSeed :: [[Mapping]] -> Int -> Int
runSeed mss s = foldl' runMap s mss

-- part 2

parseRanges :: [String] -> [Range]
parseRanges ss =
    let nums = map read $ tail $ words $ head ss
        skip [x] = [x]
        skip (x:xs) = x : skip (tail xs)
        pairs = skip $ zip nums $ tail nums
        range (a,b) = (a, a + (b - 1))
    in  foldl' (\acc p -> range p : acc) [] pairs

intersect :: Mapping -> (Bool, Range) -> [(Bool, Range)]
intersect (m1,m2,offset) (b, (r1,r2))
    | b                    = [(True, (r1,r2))]
    | m1 <= r1 && r2 <= m2 = middle
    | m1 <= r1 && r1 <= m2 = bottom
    | m1 <= r2 && r2 <= m2 = top
    | otherwise            = [(False, (r1,r2))]
    where middle = [(True, (r1 + offset, r2 + offset))]
          bottom = [(True, (r1 + offset, m2 + offset)), (False, (m2 + 1, r2))]
          top = [(False, (r1, m1 - 1)), (True, (m1 + offset, r2 + offset))]

--runRanges :: [[Mapping]] -> [Range] -> Int
runRanges mss rs =
    let inner = foldl' (\acc m -> concatMap (intersect m) acc)
        outer (ms:mss') acc = outer mss' $ map snd $ inner (zip (repeat False) acc) ms
        outer [] acc = acc
    --in  outer mss rs
    in  minimum $ map fst $ outer mss rs
