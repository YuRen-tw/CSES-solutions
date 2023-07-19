-- Collatz 3n+1 conjecture
collatzConject :: Int -> [Int]
collatzConject 1 = [1]
collatzConject n | even n    = n : collatzConject (n `div` 2)
                 | otherwise = n : collatzConject (3 * n + 1)

main :: IO ()
main = getLine >>= putStrLn . unwords . map show . collatzConject . read

