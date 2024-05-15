import Data.Time.Clock
import Data.List (intercalate, nub, sort)

time :: IO a -> IO (a, NominalDiffTime)
time action = do
   start <- getCurrentTime
   result <- action
   end <- getCurrentTime
   let diff = diffUTCTime end start
   return (result, diff)


isValid :: Int -> [Int] -> Bool
isValid rowww qs = not $ any hasConflict $ zip [1..rowww] qs
  where
    hasConflict (r, c) = any (conflict (r, c)) $ zip [1..r-1] qs
    conflict (r1, c1) (r2, c2) = c1 == c2 || abs (r1 - r2) == abs (c1 - c2)


dupD1 :: [Int] -> [Int]
dupD1 qs = [1..length qs] >>= \i -> [head [r | (r, c) <- zip [1..] qs, c == i]]

dupD2 :: [Int] -> [Int]
dupD2 qs = map (\x -> length qs + 1 - x) (reverse qs)

rot :: [Int] -> [[Int]]
rot qs = take 4 $ iterate (rot90 (length qs)) qs
    where
        rot90 :: Int -> [Int] -> [Int]
        rot90 n xs = map (\i -> n + 1 - head [r | (r, q) <- zip [1..] xs, q == i]) [1..n]

dupUnique :: [Int] -> [[Int]]
dupUnique qs = nub $ sort $ [qs, reverse qs, map (length qs + 1 -) qs, reverse (map (length qs + 1 -) qs)] ++ [dupD1 qs, dupD2 qs] ++ rot qs >>= \r -> [r, reverse r]

backtrack :: Int -> Int -> [[Int]] -> [Int] -> [[Int]]
backtrack n roww acc queens
  | roww > n = acc ++ [queens]
  | otherwise = foldr tryPlace acc [1..n]
  where
    tryPlace col acc' = if isValid roww (col : queens)
                        then backtrack n (roww + 1) acc' (col : queens)
                        else acc'

nQueens :: Int -> [[Int]]
nQueens n = filter isFundamental $ backtrack n 1 [] []
  where
      isFundamental sol = all (> sol) (tail $ dupUnique sol)


row :: Int -> Int -> [String]
row size pos = [rowContent, horizontalLine]
  where
    rowContent = intercalate " | " $ map (\x -> if x == pos then "Q" else " ") [1..size]
    horizontalLine = replicate ((size * 4) - 1) '-'

board :: Int -> [Int] -> String
board n solution = unlines $ border : concatMap (\pos -> row n pos) solution
  where
    border = replicate ((n * 4) - 1) '-'

printBoard :: Int -> IO ()
printBoard n = do
    let solutions = map (board n) (nQueens n)
    putStrLn $ "Found " ++ show (length solutions) ++ " solutions for " ++ show n ++ "-queens problem"

main :: IO ()
main = do
    (solutions, timeElapsed) <- time $ mapM_ printBoard [1..12]
    putStrLn $ "Time elapsed: " ++ show timeElapsed
-- main = printBoard 4