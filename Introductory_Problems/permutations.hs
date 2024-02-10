beautiful :: Int -> Maybe [Int]
beautiful n | n == 1    = Just [1]
            | n <= 3    = Nothing
            | otherwise = Just $ [2, 4 .. n] ++ [1, 3 .. n]

main :: IO ()
main = do
    n <- read <$> getLine
    case beautiful n of
        Nothing -> putStrLn "NO SOLUTION"
        Just xs -> putStrLn . unwords $ map show xs
