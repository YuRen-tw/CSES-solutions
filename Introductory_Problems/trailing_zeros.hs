trailZeros :: Int -> Int
trailZeros 0 = 0
trailZeros n = n `div` 5 + trailZeros (n `div` 5)

main :: IO ()
main = interact $ show . trailZeros . read
