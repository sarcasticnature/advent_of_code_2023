import System.Environment (getArgs)
import System.IO
import Data.Char (ord)
import Data.List (foldl')
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let contents' = parse $ init contents
    putStrLn "Part 1:"
    print $ sum $ hash contents'
    putStrLn "Part 2:"
    print $ power $ runLenses contents'

type Lens = (String, Int)
type BoxMap = IntMap (Seq Lens)

parse :: String -> [String]
parse s =
    let f c (acc, tmp) = if c == ',' then (tmp : acc, []) else (acc, c : tmp)
        (most, rest) = foldr f ([], []) s
    in  rest:most

hash :: [String] -> [Int]
hash = map (foldl' f 0)
    where f acc c = flip mod 256 $ (acc + ord c) * 17

runLenses :: [String] ->  BoxMap
runLenses xs =
    let split = span (\c -> c /= '=' && c /= '-')
        prune k ls = case Seq.findIndexL (pred k) ls of
            Nothing -> Just ls
            Just i  -> Just (Seq.deleteAt i ls)
        rm m h k = IntMap.update (prune k) h m
        pred k (k',_) = k == k'
        update k v Nothing = Just (Seq.singleton (k,v))
        update k v (Just ls) = case Seq.findIndexL (pred k) ls of
            Nothing -> Just (ls |> (k,v))
            Just i  -> Just (Seq.update i (k,v) ls)
        insert m h k v = IntMap.alter (update k v) h m
        f acc (s) = let (k, vs) = split s
                        v = read $ tail vs :: Int
                        h = head $ hash [k]
                    in  if head vs == '-'
                        then rm acc h k
                        else insert acc h k v
    in  foldl' f IntMap.empty xs

power :: BoxMap -> Int
power m =
    let f' h ls acc = pow ls + acc
            where f i (_,v) acc = (h + 1) * (i + 1) * v + acc
                  pow ls = Seq.foldrWithIndex f 0 ls
    in  IntMap.foldrWithKey f' 0 m
