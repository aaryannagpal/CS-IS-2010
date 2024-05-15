import Data.List (intercalate, permutations, nub, sort)

isValid :: [Int] -> Bool {- Check if the solution has no conflicts -}
isValid qs = not $ any hasConflict $ zip [1..] qs
  where
    hasConflict (r, c) = any (conflict (r, c)) $ zip [1..r-1] qs
    conflict (r1, c1) (r2, c2) = c1 == c2 || abs (r1 - r2) == abs (c1 - c2)

dupD1 :: [Int] -> [Int]
dupD1 qs = [1..length qs] >>= \i -> [head [r | (r, c) <- zip [1..] qs, c == i]]

dupD2 :: [Int] -> [Int]
dupD2 qs = map (\x -> length qs + 1 - x) (reverse qs)

dupUnique :: [Int] -> [[Int]]
dupUnique qs = nub $ sort $ [qs, reverse qs, map (length qs + 1 -) qs, reverse (map (length qs + 1 -) qs)] ++ [dupD1 qs, dupD2 qs] ++ rot qs >>= \r -> [r, reverse r]

rot :: [Int] -> [[Int]]
rot qs = take 4 $ iterate (rot90 (length qs)) qs
    where
        rot90 :: Int -> [Int] -> [Int]
        rot90 n xs = map (\i -> n + 1 - head [r | (r, q) <- zip [1..] xs, q == i]) [1..n]

nQueens :: Int -> [[Int]]
nQueens n = filter isFundamental $ filter isValid $ permutations [1..n]
-- nQueens n = filter isValid $ permutations [1..n]
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
    -- putStrLn $ intercalate "\n\n" solutions

-- Example usage
main :: IO ()
-- from 1 to 12
main = mapM_ printBoard [1..12]
-- main = printBoard 4