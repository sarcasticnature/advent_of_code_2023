import System.Environment (getArgs)
import System.IO
import Control.Parallel
import qualified Data.Vector.Unboxed as VU
import Data.Word
import Data.Bits

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let contents' = lines contents
    putStrLn "Part 1:"
    putStrLn contents
    --putStrLn $ unlines $ energyToList $ (\(d,_,ev) -> (d,ev)) $ parseGrid $ contents'
    putStrLn $ unlines $ energyToList $ runBeam $ parseGrid contents'
    --putStrLn "Part 2:"

type EnergyVec = VU.Vector Word8
type GridVec = VU.Vector Char
type Dimensions = (Int,Int)

data Dir = N | S | E | W

parseGrid :: [String] -> (Dimensions, GridVec, EnergyVec)
parseGrid ss =
    let height = length ss
        width = length $ head ss
        grid = VU.fromList $ concat ss
        energy = VU.replicate (height * width) 0x0
    in  ((width,height), grid, energy)

uncat :: Int -> [a] -> [[a]]
uncat _ [] = []
uncat i xs = take i xs : uncat i (drop i xs)

energyToList :: (Dimensions, EnergyVec) -> [String]
energyToList ((w,_),ev) = map (map f) $ uncat w $ VU.toList ev
    where f x = if x .|. 0x0 == 0x0 then '.' else '#'

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
merge = VU.zipWith (.|.)

dirBit :: Dir -> Word8
dirBit d = case d of
    N -> 0x1
    S -> 0x2
    E -> 0x4
    W -> 0x8

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
            | c == '|'  = concatMap (move (x,y)) $ bar d
            | c == '-'  = concatMap (move (x,y)) $ dash d
            where c = gv VU.! (x + (w * y))
        steps ev [] = ev
        steps ev ((x,y,d):zs) = if (/= 0) $ oldCell .&. dirBit d then ev else go
            where idx = w * y + x
                  oldCell = ev VU.! idx
                  ev' = ev VU.// [(idx, (.|.) (dirBit d) oldCell)]
                  ev'' = steps ev' zs
                  ev''' = steps ev' $ dirs (x,y,d)
                  go = ev'' `par` ev''' `pseq` merge ev'' ev'''
    in ((w,h), steps ev [(0,0,E)])
