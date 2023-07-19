-- OEIS A172132
a172132 :: [Int]
a172132 = recSeq 0 6 28 96 252
  where recSeq a0 a1 a2 a3 a4 = a0 : recSeq a1 a2 a3 a4 (recur a0 a1 a2 a3 a4)
        recur a0 a1 a2 a3 a4 = a0 - 5*a1 + 10*a2 - 10*a3 + 5*a4
 
main :: IO ()
main = interact $ unlines . map show . flip take a172132 . read
