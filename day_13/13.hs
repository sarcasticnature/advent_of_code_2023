import System.Environment (getArgs)
import System.IO
import Data.List (transpose)
import Data.Maybe (catMaybes)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let contents' = lines contents
    putStrLn "Part 1:"
    let hor = (*) 100 $ sum $ catMaybes $ map mirror $ patterns $ contents'
    let vert = sum $ catMaybes $ map (mirror . transpose) $ patterns $ contents'
    print $ hor + vert
    putStrLn "Part 2:"
    let hor' = (*) 100 $ sum $ catMaybes $ map smudge $ patterns $ contents'
    let vert' = sum $ catMaybes $ map (smudge . transpose) $ patterns $ contents'
    print $ hor' + vert'

type Pattern = [String]

patterns :: [String] -> [Pattern]
patterns xs =
    let f x (acc, tmp) = if x == "" then (tmp : acc, []) else (acc, x : tmp)
        (most, rest) = foldr f ([], []) xs
    in  rest:most

mirror :: Pattern -> Maybe Int
mirror p =
    let height = length p
        valid ps = and $ map (uncurry (==)) ps
        combo i = zip (concat $ reverse $ take i p) (concat $ drop i p)
        res = filter (snd) $ zip [1..] $ map (valid . combo) [1..height - 1]
    in  case res of []      -> Nothing
                    [(x,_)] -> Just x
                    _       -> error "too many mirror results"
                    --_       -> Nothing

smudge :: Pattern -> Maybe Int
smudge p =
    let height = length p
        valid ps = (==) 1 $ length $ filter (not . id) $ map (uncurry (==)) ps
        combo i = zip (concat $ reverse $ take i p) (concat $ drop i p)
        res = filter (snd) $ zip [1..] $ map (valid . combo) [1..height - 1]
    in  case res of []      -> Nothing
                    [(x,_)] -> Just x
                    _       -> error "too many mirror results"
                    --_       -> Nothing
