import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

showBS :: Show a => a -> C.ByteString
showBS = C.pack . show

tasks :: Show a => ([Int] -> [a]) -> IO()
tasks mapFn = C.interact
    $ C.unlines . map showBS . mapFn . map readInt . drop 1 . C.words

---

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


map2 :: (a -> a -> b) -> [a] -> [b]
map2 f (x:y:xs) = f x y : map2 f xs
map2 _ _        = []

main :: IO ()
main = tasks $ map2 (\a b -> FieldElem a ^ b)
