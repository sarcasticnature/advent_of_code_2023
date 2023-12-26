import System.Environment (getArgs)
import System.IO
import Data.List (sortOn, sortBy)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    let contents' = lines contents
    putStrLn "Part 1:"
    --putStrLn contents
    --let scores = sortOn (\(t,_,_) -> t) $ map (score . parseHand) contents'
    let scores = sortBy sorter $ map (score . parseHand) contents'
    print $ sum $ zipWith (*) [1..] $ map (\(_,_,n) -> n) scores
    putStrLn "Part 2:"
    let scores' = sortBy sorter $ map (score' . parseHand') contents'
    mapM_ print scores'
    print $ sum $ zipWith (*) [1..] $ map (\(_,_,n) -> n) scores'

data Type = High | Pair | TwoPair | Three | Full | Four | Five
            deriving (Eq, Ord, Show)

parseCard :: Char -> Int
parseCard c
    | c == 'A'  = 14
    | c == 'K'  = 13
    | c == 'Q'  = 12
    | c == 'J'  = 11
    | c == 'T'  = 10
    | otherwise = read [c]

parseHand :: String -> ([Int], Int)
parseHand s =
    let [cs, ns] = words s
        n = read ns
        is = map parseCard cs
    in  (is, n)

scoreMap :: IntMap Int -> Type
scoreMap im
    | max' == 5 = Five
    | max' == 4 = Four
    | max' == 3 = if min' == 2 then Full else Three
    | max' == 2 = if IntMap.size im == 3 then TwoPair else Pair
    | otherwise = High
    where max' = maximum im
          min' = minimum im

score :: ([Int], Int) -> (Type, [Int], Int)
score (cs, x) =
    let f = IntMap.alter f'
            where f' m = case m of Nothing -> Just 1
                                   Just n  -> Just (n + 1)
        im = foldr f IntMap.empty cs
    in  (scoreMap im, cs, x)

sorter :: (Type, [Int], Int) -> (Type, [Int], Int) -> Ordering
sorter (t1,c1,_) (t2,c2,_) = case t1 `compare` t2 of
    GT -> GT
    LT -> LT
    EQ -> c1 `compare` c2

-- part 2

parseJoke :: Char -> Int
parseJoke c
    | c == 'A'  = 14
    | c == 'K'  = 13
    | c == 'Q'  = 12
    | c == 'J'  = 1
    | c == 'T'  = 10
    | otherwise = read [c]

noJoke :: IntMap Int -> Type
noJoke im
    | max' == 5 = Five
    | max' == 4 = Four
    | max' == 3 = if min' == 2 then Full else Three
    | max' == 2 = if IntMap.size im == 3 then TwoPair else Pair
    | otherwise = High
    where max' = maximum im
          min' = minimum im

scoreJoke :: IntMap Int -> Type
scoreJoke im = case noJoke im of
    Five -> Five
    Four -> if j > 0 then Five else Four
    Full -> if j == 2 || j == 3 then Five else Full
    Three -> three
    TwoPair -> twopair
    Pair -> pair
    High -> high
    where j = IntMap.findWithDefault 0 1 im
          three | j == 0 = Three
                | j == 1 = Four
                | j == 2 = Five
                | j == 3 = Four
          twopair | j == 0 = TwoPair
                  | j == 1 = Full
                  | j == 2 = Four
          pair | j == 0 = Pair
               | j == 1 = Three
               | j == 2 = Three
               | j == 3 = Five
          high | j == 0 = High
               | j == 1 = Pair
               | j == 2 = Three
               | j == 3 = Four
               | j == 4 = Five

score' :: ([Int], Int) -> (Type, [Int], Int)
score' (cs, x) =
    let f = IntMap.alter f'
            where f' m = case m of Nothing -> Just 1
                                   Just n  -> Just (n + 1)
        im = foldr f IntMap.empty cs
    in  (scoreJoke im, cs, x)

parseHand' :: String -> ([Int], Int)
parseHand' s =
    let [cs, ns] = words s
        n = read ns
        is = map parseJoke cs
    in  (is, n)
