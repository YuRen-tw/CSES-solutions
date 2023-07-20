-- The prime field mod 1000000007
newtype PrimeField = FieldElem { toInt :: Int }

fromInt :: Int -> PrimeField 
fromInt = FieldElem . (`mod` 1000000007)

instance Show PrimeField where
    show = show . toInt

instance Num PrimeField where
    (FieldElem a) + (FieldElem b) = fromInt (a + b)
    (FieldElem a) - (FieldElem b) = fromInt (a - b)
    (FieldElem a) * (FieldElem b) = fromInt (a * b)
    abs    = id
    signum = const 1
    fromInteger = fromInt . fromInteger

main :: IO ()
main = interact $ show . (FieldElem 2 ^) . (read :: String -> Int)
