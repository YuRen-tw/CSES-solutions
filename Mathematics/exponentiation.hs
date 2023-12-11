import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

showBS :: Show a => a -> C.ByteString
showBS = C.pack . show

chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f xs = y : chop f ys
  where (y, ys) = f xs

tasks :: Show a => ([Int] -> (a, [Int])) -> IO()
tasks f = C.interact
    $ C.unlines . map showBS . chop f . map readInt . drop 1 . C.words

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

main :: IO ()
main = tasks $ \(a:b:xs) -> (FieldElem a ^ b, xs)
