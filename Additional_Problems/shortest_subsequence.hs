import Data.Bits ((.&.), (.|.))

solve :: Int -> [Char] -> [Char]
solve 14 ('A':cs) = 'A' : solve 0 cs
solve 13 ('C':cs) = 'C' : solve 0 cs
solve 11 ('G':cs) = 'G' : solve 0 cs
solve  7 ('T':cs) = 'T' : solve 0 cs
solve  n ('A':cs) = solve (n .|. 1) cs
solve  n ('C':cs) = solve (n .|. 2) cs
solve  n ('G':cs) = solve (n .|. 4) cs
solve  n ('T':cs) = solve (n .|. 8) cs
solve  n [] | n .&. 1 == 0 = "A"
            | n .&. 2 == 0 = "C"
            | n .&. 4 == 0 = "G"
            | otherwise    = "T"
solve _ _ = "X"  -- impossible

main :: IO ()
main = interact $ solve 0
