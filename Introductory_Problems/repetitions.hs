solve :: Int -> Int -> Char -> [Char] -> Int
solve mx _   _    []     = mx
solve mx cnt prev (c:cs)
    | prev == c = solve (max (cnt+1) mx) (cnt+1) c cs
    | otherwise = solve mx 1 c cs

main :: IO ()
main = interact $ show . solve 1 1 '-'
