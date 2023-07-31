jump :: Int -> Int -> [Int]
jump _   0 = []
jump n   1 = [n]
jump n len = n : n+1 : jump (n + 4) (len - 2)

twoSets :: Int -> Maybe (Int, [Int], Int, [Int])
twoSets n
    | n `mod` 4 == 3 = Just ( m + 1
                            , jump 1 (m + 1)
                            , m
                            , jump 3 m
                            )
    | n `mod` 4 == 0 = Just ( m
                            , 1 : jump 4 (m - 1)
                            , m
                            , jump 2 m
                            )
    | otherwise      = Nothing
  where m = n `div` 2

main :: IO ()
main = do
    n <- read <$> getLine
    case twoSets n of
        Just (a, xs, b, ys) -> do
            putStrLn "YES"
            print a
            putStrLn . unwords $ map show xs
            print b
            putStrLn . unwords $ map show ys
        Nothing -> putStrLn "NO"
