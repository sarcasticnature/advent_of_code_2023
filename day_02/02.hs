import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    let max = MaxCubes 12 13 14
    print $ sum $ map (isPossible max . parseGame) $ lines contents
    putStrLn "Part 2:"
    print $ sum $ map (cubeProduct . maxPossible . parseGame) $ lines contents

-- data
data Cube = Red Int
           | Green Int
           | Blue Int
           deriving (Show)
type Set = [Cube]
type Game = (Int, [Set])

data MaxCubes = MaxCubes { redMax :: Int
                         , greenMax :: Int
                         , blueMax :: Int
                         } deriving (Show)

-- functions

split :: Char -> String -> [String]
split _ [] = []
split delim xs =
    let f c (out, tmp) = if c /= delim then (out, c:tmp) else (tmp:out, [])
        (most, rest) = foldr f ([], []) xs
    in  filter (/= "") $ rest:most

parseCube :: String -> Cube
parseCube xs =
    let xs' = words xs
        cnt = read $ head xs' :: Int
        color = last xs'
        build n "red" = Red n
        build n "green" = Green n
        build n "blue" = Blue n
    in  build cnt color

parseGame :: String -> Game
parseGame xs = 
    let i = read $ init $ words xs !! 1
        xs' = tail $ dropWhile (/= ':') xs
        sets = split ';' xs'
        sets' = map (split ',') sets
        sets'' = map (map parseCube) sets'
    in  (i, sets'')

possible :: MaxCubes -> Cube -> Bool
possible max color = case color of
    Red n   -> n <= redMax max
    Green n -> n <= greenMax max
    Blue n  -> n <= blueMax max

isPossible :: MaxCubes -> Game -> Int
isPossible max (i, set) =
    let set' = concat set
        ps = map (possible max) set'
    in  if and ps then i else 0

maxCube :: MaxCubes -> Cube -> MaxCubes
maxCube (MaxCubes r g b) c = case c of
    Red n   -> MaxCubes (max n r) g b
    Green n -> MaxCubes r (max n g) b
    Blue n  -> MaxCubes r g (max n b)

maxPossible :: Game -> MaxCubes
maxPossible (_, sets) =
    let sets' = concat sets
    in  foldr (flip maxCube) (MaxCubes 0 0 0) sets'

cubeProduct :: MaxCubes -> Int
cubeProduct (MaxCubes r g b) = r * g * b
