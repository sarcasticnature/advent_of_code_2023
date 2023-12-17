import System.Environment (getArgs)
import System.IO
import Data.List (transpose, foldl')
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let contents' = lines contents
    putStrLn "Part 1:"
    print $ load $ tilt N $ contents'
    putStrLn "Part 2:"
    putStrLn contents
    --let cycles = take 4000000000 $ cycle [N,W,S,E]
    putStrLn $ unlines $ snd $ iterate cycleCache (HM.empty, contents') !! 1000000

data Direction = N | S | E | W
type Cache = HM.HashMap C.ByteString C.ByteString

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

runCycle :: [String] -> [String]
runCycle ss = foldl' (flip tilt) ss [N,W,S,E]

cycleCache :: (Cache, [String]) -> (Cache, [String])
cycleCache (cache,ss) = case HM.lookup bs cache of
    Just bs' -> (cache, lines $ C.unpack bs')
    Nothing  -> (HM.insert bs (C.pack $ unlines ss') cache, ss')
    where bs = C.pack $ unlines ss
          ss' = runCycle ss
