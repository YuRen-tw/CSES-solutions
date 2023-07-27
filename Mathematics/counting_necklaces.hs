-- The prime field mod 1000000007
newtype PrimeField = FieldElem { toInt :: Int }

fromInt :: Int -> PrimeField 
fromInt = FieldElem . (`mod` 1000000007)

instance Num PrimeField where
    (FieldElem a) + (FieldElem b) = fromInt (a + b)
    (FieldElem a) - (FieldElem b) = fromInt (a - b)
    (FieldElem a) * (FieldElem b) = fromInt (a * b)
    abs    = id
    signum = const 1
    fromInteger = fromInt . fromInteger

extEuclid :: Int -> Int -> (Int, Int, Int)
extEuclid a 0 = (a, 1, 0)
extEuclid a b = (g, n, m - n*q)
  where (q, r)    = a `divMod` b
        (g, m, n) = extEuclid b r

reciprocal :: PrimeField -> PrimeField
reciprocal (FieldElem a) = FieldElem b
  where (_, _, b) = extEuclid 1000000007 a

-- Euler's phi function
phiProc :: Int -> Int -> Int -> Int -> Int
phiProc _   _ _     1 = 1
phiProc bnd p prevP n
    | p*p > bnd = n-1
    | otherwise = case n `quotRem` p of
       (m, 0) -> if p == prevP
                 then p     * phiProc bnd p p m
                 else (p-1) * phiProc bnd p p m
       _      -> phiProc bnd (p+1) p n

phi :: Int -> Int
phi n = phiProc n 2 1 n

divisors :: Int -> [(Int, Int)]
divisors n = divs n 1
  where divs m x | x*x > m   = []
                 | x*x == m  = [(x, x)]
                 | otherwise = case m `quotRem` x of
                     (q, 0) -> (x, q) : (q, x) : divs m (x+1)
                     _      -> divs m (x+1)

{-
  Burnside's lemma    Euler's phi function
      \                  /
       \  n-1 gcd(n,k)  /    d
  n Ans =  ∑ m         =  ∑ m φ(n/d)
          k=1            d|n
-}
solve :: Int -> Int -> Int
solve n k = toInt . (nInv *) . sum . map summand $ divisors n
  where summand (a, b) = fromInt k ^ a * FieldElem (phi b)
        nInv = reciprocal (fromInt n)

main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine
    print $ solve n k
