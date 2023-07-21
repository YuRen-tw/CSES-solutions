-- Tower of Hanoi
hanoi :: Int -> Char -> Char -> Char -> [[Char]]
hanoi 1 a b _ = [[a, ' ', b]]
hanoi n a b c = hanoi (n-1) a c b ++ [[a, ' ', b]] ++ hanoi (n-1) c b a

main :: IO ()
main = do
    n <- read <$> getLine
    print $ (2^n - 1 :: Int)  -- number of moves
    putStr . unlines $ hanoi n '1' '3' '2'
