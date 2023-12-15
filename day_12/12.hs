import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ map combos $ parse $ lines contents
    putStrLn "\n"
    print $ combos' $ last $ parse $ lines contents
    --putStrLn "Part 2:"

parse :: [String] -> [(String, [Int])]
parse = map (p . words)
    where p (str:nums) = (str, map read $ words [if c == ',' then ' ' else c | c <- head nums])

combos :: (String, [Int]) -> Int
combos (ss,[]) = if '#' `elem` ss then 0 else 1
combos ("",_) = 0
combos (ss,n:ns) =
    let springFit ss' = length ss' == n && all (\c -> c == '#' || c == '?') ss'
        dropped ss' = if d == [] then [] else tail d
            where d = dropWhile (== '#') ss'
        peek = combos (dropped $ drop n ss, ns)
        hasSpace = case drop n ss of []    -> True
                                     '.':_ -> True
                                     '?':_ -> True
                                     _     -> False
        valid = if springFit (take n ss) && hasSpace
                then peek
                else 0
        munch = case head ss of '#' -> combos (dropped ss, n:ns)
                                _   -> combos (tail ss, n:ns)
    in  munch + valid

-- debug

combos' :: (String, [Int]) -> [(String, [Int])]
combos' (ss,[]) = if '#' `elem` ss then [] else [(ss, [])]
combos' ("",_) = []
combos' (ss,n:ns) =
    let springFit ss' = length ss' == n && all (\c -> c == '#' || c == '?') ss'
        dropped = if d == [] then [] else tail d
            where d = dropWhile (== '#') ss
        peek = combos' (drop (n + 1) ss, ns)
        hasSpace = case drop n ss of []    -> True
                                     '.':_ -> True
                                     '?':_ -> True
                                     _     -> False
        valid = if springFit (take n ss) && peek /= [] && hasSpace
                then [(ss, n:ns)]
                else []
        munch = case head ss of '#' -> combos' (dropped, n:ns)
                                _   -> combos' (tail ss, n:ns)
    in  munch ++ valid

