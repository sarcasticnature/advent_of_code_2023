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
    let (begin,end) = fixCycle contents'
    print (begin,end)
    let n = (1000000000 - begin) `mod` (end - begin)
    print $ load $ iterate runCycle contents' !! (n + begin)

data Direction = N | S | E | W
type Cache = HM.HashMap C.ByteString (Int, C.ByteString)

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

cycleCache :: (Int, Cache, [String]) -> (Int, Cache, [String])
cycleCache (i, cache,ss) = case HM.lookup bs cache of
    Just (_,bs') -> (i', cache, lines $ C.unpack bs')
    Nothing  -> (i', HM.insert bs (i', (C.pack $ unlines ss')) cache, ss')
    where bs = C.pack $ unlines ss
          ss' = runCycle ss
          i' = i + 1

fixCycle :: [String] -> (Int, Int)
fixCycle ss = fc (0, HM.empty, ss)
    where fc (i, c, ss') = let (i', c', ss'') = cycleCache (i, c, ss')
                           in  if c == c'
                               then (fst $ c HM.! (C.pack $ unlines ss'), i')
                               else fc (i + 1, c', ss'')
