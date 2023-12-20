import System.Environment (getArgs)
import System.IO
import Control.Parallel
import qualified Data.Vector.Unboxed as VU

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let contents' = lines contents
    putStrLn "Part 1:"
    putStrLn contents
    --putStrLn $ unlines $ energyToList $ (\(d,_,ev) -> (d,ev)) $ parseGrid $ contents'
    putStrLn $ unlines $ energyToList $ runBeam $ parseGrid $ contents'
    --putStrLn "Part 2:"

type EnergyVec = VU.Vector Bool
type GridVec = VU.Vector Char
type Dimensions = (Int,Int)

data Dir = N | S | E | W

parseGrid :: [String] -> (Dimensions, GridVec, EnergyVec)
parseGrid ss =
    let height = length ss
        width = length $ head ss
        grid = VU.fromList $ concat ss
        energy = VU.replicate (height * width) False
    in  ((width,height), grid, energy)

uncat :: Int -> [a] -> [[a]]
uncat _ [] = []
uncat i xs = take i xs : uncat i (drop i xs)

energyToList :: (Dimensions, EnergyVec) -> [String]
energyToList ((w,_),ev) = map (map f) $ uncat w $ VU.toList ev
    where f b = if b then '#' else '.'

fSlash :: Dir -> Dir
fSlash d = case d of
    N -> E
    S -> W
    E -> N
    W -> S

bSlash :: Dir -> Dir
bSlash d = case d of
    N -> W
    S -> E
    E -> S
    W -> N

bar d = case d of
    N -> [N]
    S -> [S]
    _ -> [N,S]

dash d = case d of
    E -> [E]
    W -> [W]
    _ -> [E,W]

merge :: EnergyVec -> EnergyVec -> EnergyVec
merge a b = VU.zipWith (||) a b

runBeam :: (Dimensions, GridVec, EnergyVec) -> (Dimensions, EnergyVec)
runBeam ((w,h), gv, ev) =
    let clip max x
            | x < 0     = Nothing
            | x > max   = Nothing
            | otherwise = Just x
        clipxy (x,y) = case (clip (w - 1) x, clip (h - 1) y) of
            (Nothing, _)       -> Nothing
            (_, Nothing)       -> Nothing
            (Just x', Just y') -> Just (x',y')
        mToList Nothing _      = []
        mToList (Just (x,y)) d = [(x,y,d)]
        move (x,y) d = case d of N -> mToList (clipxy (x, y - 1)) d
                                 S -> mToList (clipxy (x, y + 1)) d
                                 E -> mToList (clipxy (x + 1, y)) d
                                 W -> mToList (clipxy (x - 1, y)) d
        dirs (x,y,d)
            | c == '.'  = move (x,y) d
            | c == '/'  = move (x,y) $ fSlash d
            | c == '\\' = move (x,y) $ bSlash d
            | c == '|'  = concat $ map (move (x,y)) $ bar d
            | c == '-'  = concat $ map (move (x,y)) $ dash d
            where c = gv VU.! (x + (w * y))
        steps ev [] = ev
        steps ev ((x,y,d):zs) = ev'' `par` ev''' `pseq` (merge ev'' ev''')
            where ev' = ev VU.// [(x * y, True)]
                  ev'' = steps ev' zs
                  ev''' = steps ev' $ dirs (x,y,d)
    in ((w,h), steps ev [(0,0,E)])
