import System.Environment (getArgs)
import System.IO
import Data.List (transpose, foldl')

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let contents' = lines contents
    putStrLn "Part 1:"
    putStrLn contents
    putStrLn $ unlines $ tilt N $ contents'
    print $ load $ tilt N $ contents'
    --putStrLn "Part 2:"

data Direction = N | S | E | W

shift :: String -> String
shift s =
    let f (s', n) '.' = ('.' : s', n)
        f (s', n) 'O' = ('.' : s', n + 1)
        f (s', n) '#' = ('#' : os ++ rest, 0)
            where os = replicate n 'O'
                  rest = drop n s'
        (most, n) = foldl' f ("", 0) s
    in  reverse $ replicate n 'O' ++ drop n most

tilt :: Direction -> [String] -> [String]
tilt N = transpose . map reverse . map shift . map reverse . transpose
tilt S = transpose . map shift . transpose
tilt E = map shift
tilt W = map reverse . map shift . map reverse

load :: [String] -> Int
load ss = sum $ map (sum . z . reverse) $ transpose ss
    where z = zipWith (\i c -> if c == 'O' then i else 0) [1..]
