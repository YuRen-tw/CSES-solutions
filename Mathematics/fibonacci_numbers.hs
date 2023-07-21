-- The 2x2 matrix mod 1000000007
newtype Matrix = Mat { getEentries :: ((Int, Int), (Int, Int)) }

prime :: Int
prime = 1000000007

fromEentries :: ((Int, Int), (Int, Int)) -> Matrix
fromEentries ((a, b), (c, d)) =
    Mat ( (a `mod` prime, b `mod` prime)
        , (c `mod` prime, d `mod` prime)
        )

instance Num Matrix where
    (Mat ((a1, b1), (c1, d1))) + (Mat ((a2, b2), (c2, d2))) =
        fromEentries ((a1+a2, b1+b2), (c1+c2, d1+d2))
    (Mat ((a1, b1), (c1, d1))) - (Mat ((a2, b2), (c2, d2))) =
        fromEentries ((a1-a2, b1-b2), (c1-c2, d1-d2))
    (Mat ((a1, b1), (c1, d1))) * (Mat ((a2, b2), (c2, d2))) =
        fromEentries ( (a1*a2 + c1*b2, b1*a2 + d1*b2)
                     , (a1*c2 + c1*d2, b1*c2 + d1*d2)
                     )
    abs    = id
    signum = const $ Mat ((1, 0), (0, 1))
    fromInteger n = fromEentries ((m, 0), (0, m))
      where m = fromInteger n

fib :: Int -> Int
fib n = fst . snd . getEentries $ fibMat ^ n
  where fibMat = Mat ((0, 1), (1, 1))

main :: IO ()
main = interact $ show . fib . read
