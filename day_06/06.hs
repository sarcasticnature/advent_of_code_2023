import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    let times = map read $ tail $ words $ head $ lines contents
    let distances = map read $ tail $ words $ last $ lines contents
    print $ product $ zipWith waysToWin times distances
    putStrLn "Part 2:"
    let times' = read $ filter (/= ' ') $ unwords $ tail $ words $ head $ lines contents :: Int
    let distances' = read $ filter (/= ' ') $ unwords $ tail $ words $ last $ lines contents :: Int
    print $ waysToWin times' distances'

-- *unsafe* quadratic equation (result could be NaN)
quad :: Floating a => a -> a -> a -> (a, a)
quad a b c = (((-b) - sqrt desc) / (2 * a), ((-b) + sqrt desc) / (2 * a))
    where desc = b ^^ 2 - 4.0 * a * c

waysToWin :: Int -> Int -> Int
waysToWin t d = (max'' - min'') + 1
    where (m1,m2) = quad (-1.0 :: Double) (fromIntegral t) (fromIntegral (-d))
          dist t t' = t' * t - t' ^ 2
          max' = floor $ max m1 m2
          min' = ceiling $ min m1 m2
          max'' = if d == dist t max' then max' - 1 else max'
          min'' = if d == dist t min' then min' + 1 else min'
