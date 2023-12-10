import System.Environment (getArgs)
import System.IO

main = do
    filename_list <- getArgs
    let filename = head filename_list
    contents <- readFile filename
    putStrLn "Part 1:"
    print $ sum $ map score $ parse $ lines contents
    putStrLn "Part 2:"
    print $ cards $ zip (repeat 1) $ parse $ lines contents

parse :: [String] -> [([Int], [Int])]
parse xs =
    let xs' = map (tail . dropWhile (/= ':')) xs
        win = map (map read . words . takeWhile (/= '|')) xs'
        num = map (map read . tail . words . dropWhile (/= '|')) xs'
    in  zip win num

score :: ([Int], [Int]) -> Int
score (wins, nums) = foldr f 0 nums
    where f n acc = if n `elem` wins then if acc == 0 then 1 else acc * 2 else acc

count :: ([Int], [Int]) -> Int
count (wins, nums) = foldr (\n acc -> if n `elem` wins then acc + 1 else acc) 0 nums

cards :: [(Int, ([Int], [Int]))] -> Int
cards [] = 0
cards (x:xs) =
    let next = count $ snd x
        cnt = fst x
        (wins, rest) = splitAt next xs
        wins' = map (\(a, b) -> (a + cnt, b)) wins
    in  cnt + cards (wins' ++ rest)
