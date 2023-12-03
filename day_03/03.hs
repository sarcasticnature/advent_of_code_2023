import System.Environment (getArgs)
import System.IO
import Data.List (nub)
import Data.Char (isDigit)
import Data.Map (Map)
import qualified Data.Map as Map

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ sum $ getValidNums $ markValidCells $ lines contents
    putStrLn "Part 2:"
    -- TODO: only iterate over the contents once
    let pm = createPartMap (length $ head $ lines contents) $ getPartNums $ indexCells $ lines contents
    print $ sumGears pm $ indexCells $ lines contents
    --let spots = concat $ gearSpots pm $ indexCells $ lines contents
    --print $ spotCells spots $ indexCells $ lines contents
    --print $ singleGearIdx pm $ indexCells $ lines contents
    --print $ getPartNums $ indexCells $ lines contents

-- data

type Schematic = [String]
data Cell = Symbol Char
          | Digit Char
          | Dot
          deriving (Show, Eq)
-- TODO: actually use this
type Index = (Int, Int)

data PartNumber = PartNumber (Int, Int) Int deriving (Show, Eq)
type PartMap = Map (Int,Int) PartNumber
-- functions

readCell :: Char -> Cell
readCell c
    | c == '.'  = Dot
    | isDigit c = Digit c
    | otherwise = Symbol c

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
        f (a,b) acc = case getAtIndex a b s of Symbol _ -> True
                                               _        -> acc
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

-- part 2
indexCells :: Schematic -> [[((Int,Int), Cell)]]
indexCells s =
    let check = checkAdj s
        rows = length s
        cols = length $ head s
        cells = map (map readCell) s
        repeater n = take cols $ zip [0..] $ repeat n
        idxs = map repeater [0..rows]
        tupleZip (l,r) = zip l r
    in  zipWith zip idxs cells

markNums :: ((Int,Int), Cell) -> ([PartNumber], String) -> ([PartNumber], String)
markNums ((x, y), c) acc@(out, str) = case (c, str) of
    (Digit c, _) -> (out, c:str)
    (_, [])      -> acc
    (_, s)       -> (pn:out, "")
                    where x' = x + 1
                          pn = PartNumber (x',y) (read s)

getPartNums :: [[((Int,Int), Cell)]] -> [[PartNumber]]
getPartNums xs =
    let ys = map (foldr markNums ([], "")) xs
        finish (i,(most, s)) = if s /= "" then PartNumber (0,i) (read s): most else most
    in  map finish $ zip [0..] ys

digits :: Int -> Int
digits = length . show

createPartMap :: Int -> [[PartNumber]] -> PartMap
createPartMap limit pns =
    let pns' = concat pns
        f pn@(PartNumber (x,y) n) = [((x',y), pn) | x' <- [x..end]]
                                    where end = min limit $ x + digits n - 1
    in  Map.fromList $ concatMap f pns'

checkGearCount :: PartMap -> (Int,Int) -> Int
checkGearCount pm (x,y) =
    let xlist = [(x-1)..(x+1)]
        ylist = [(y-1)..(y+1)]
        idxs = [(a,b) | a <- xlist , b <- ylist]
        --f idx acc = case Map.lookup idx pm of Just pn -> pn:acc
        f idx acc = case Map.lookup idx pm of Just pn -> if notElem pn acc then pn:acc else acc
                                              Nothing -> acc
        pns = foldr f [] idxs
        --pns' = nub pns
        --f' (PartNumber _ n) acc = if acc == 0 then n else n * acc
        f' (PartNumber _ n) acc = n * acc
    --in  if length pns' == 2 then foldr f' 1 pns' else 0
    in  if length pns == 2 then foldr f' 1 pns else 0

sumGears :: PartMap -> [[((Int,Int), Cell)]] -> Int
sumGears pm xs =
    let f (xy, c) acc = case c of Symbol '*' -> acc + checkGearCount pm xy
                                  _          -> acc
    in  sum $ map (foldr f 0) xs

-- debug

--gearSpots :: PartMap -> [[((Int,Int), Cell)]] ->
gearSpots pm xs =
    let f (xy, c) acc = case c of Symbol '*' -> xy : acc
                                  _          -> acc
    in map (foldr f []) xs

gearList :: PartMap -> [[((Int,Int), Cell)]] -> [Int]
gearList pm xs =
    let f (xy, c) acc = case c of Symbol '*' -> acc + checkGearCount pm xy
                                  _          -> acc
    in  map (foldr f 0) xs

spotCells :: [Index] -> [[(Index, Cell)]] -> [Cell]
spotCells idxs xs =
    let f (idx, c) acc = if idx `elem` idxs then c:acc else acc
    in  concat $ map (foldr f []) xs

getGearCount :: PartMap -> Index -> Int
getGearCount pm (x,y) =
    let xlist = [(x-1)..(x+1)]
        ylist = [(y-1)..(y+1)]
        idxs = [(a,b) | a <- xlist , b <- ylist]
        --f idx acc = case Map.lookup idx pm of Just pn -> pn:acc
        f idx acc = case Map.lookup idx pm of Just pn -> if notElem pn acc then pn:acc else acc
                                              Nothing -> acc
        pns = foldr f [] idxs
        --pns' = nub pns
    in  length pns

listGearPns :: PartMap -> [[(Index, Cell)]] -> [Int]
listGearPns pm xs =
    let f (xy, c) acc = case c of Symbol '*' -> getGearCount pm xy : acc
                                  _          -> acc
    in  concat $ map (foldr f []) xs

singleGearIdx :: PartMap -> [[(Index, Cell)]] -> [Index]
singleGearIdx pm xs =
    let f (xy, c) acc = case c of Symbol '*' -> if getGearCount pm xy == 1 then xy : acc else acc
                                  _          -> acc
    in  concat $ map (foldr f []) xs
