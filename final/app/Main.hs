import Data.List (intercalate)
isValid :: Int -> [Int] -> Bool
isValid rowww qs = not $ any hasConflict $ zip [1..rowww] qs
  where
    hasConflict (r, c) = any (conflict (r, c)) $ zip [1..r-1] qs
    conflict (r1, c1) (r2, c2) = c1 == c2 || abs (r1 - r2) == abs (c1 - c2)

backtrack :: Int -> Int -> [[Int]] -> [Int] -> [[Int]]
backtrack n roww acc queens
  | roww > n = acc ++ [queens]
  | otherwise = foldr tryPlace acc [1..n]
  where
    tryPlace col acc' = if isValid roww (col : queens)
                        then backtrack n (roww + 1) acc' (col : queens)
                        else acc'

nQueens :: Int -> [[Int]]
nQueens n = backtrack n 1 [] []

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
main = mapM_ printBoard [1..12]
-- main = printBoard 4