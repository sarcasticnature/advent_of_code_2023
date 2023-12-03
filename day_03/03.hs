import System.Environment (getArgs)
import System.IO
import Data.Char (isDigit)

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ sum $ getValidNums $ markValidCells $ lines contents
    --putStrLn "Part 2:"

-- data

type Schematic = [String]
data Cell = Symbol
          | Digit Char
          | Dot
          deriving (Show, Eq)
type Index = (Int, Int)

-- functions

readCell :: Char -> Cell
readCell c
    | c == '.'  = Dot
    | isDigit c = Digit c
    | otherwise = Symbol

clip :: (Int,Int) -> Int -> Int
clip (min,max) x =
    let x' = if x < min then min else x
        x'' = if x' > max then max else x'
    in  x''

-- this shoud do bounds checking...
-- TODO: make Schematic the first arg so it can be curried
getAtIndex :: Int -> Int -> Schematic -> Cell
getAtIndex x y s =
    let row = s !! y
        col = row !! x
    in  readCell col

checkAdj :: Schematic -> Int -> Int -> Bool
checkAdj s x y =
    let rows = length s
        cols = length $ head s
        clipy = clip (0, rows - 1)
        clipx = clip (0, cols - 1)
        xlist = [clipx (x-1) .. clipx (x+1)]
        ylist = [clipy (y-1) .. clipy (y+1)]
        idxs = [(a,b) | a <- xlist , b <- ylist]
        f (a,b) acc = (getAtIndex a b s == Symbol) || acc
    in  foldr f False idxs

markValidCells :: Schematic -> [[(Cell, Bool)]]
markValidCells s =
    let check = checkAdj s
        rows = length s
        cols = length $ head s
        repeater n = take cols $ zip [0..] $ repeat n
        idxs = map repeater [0..rows]
        tupleZip (l,r) = zip l r
        idxSchematic = zipWith zip idxs s
        f ((x,y), c) acc = (readCell c, check x y) : acc
    in  map (foldr f []) idxSchematic

accumulateDigits :: (Cell, Bool) -> ([Int], (Bool, String)) -> ([Int], (Bool, String))
accumulateDigits (cell, b) acc@(out, (valid, str)) = case (cell, str) of
    (Digit c, _) -> (out, (valid || b, c:str))
    (_, [])      -> acc
    (_, s)       -> if valid
                    then (read s : out, (False, ""))
                    else (out, (False, ""))

getValidNums :: [[(Cell, Bool)]] -> [Int]
getValidNums xs =
    let ys = map (foldr accumulateDigits ([], (False, ""))) xs
        finish (most, (valid, str)) = if valid then read str : most else most
        zs = map finish ys
    in  concat zs
