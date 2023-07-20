import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

showBS :: (Show a) => a -> C.ByteString
showBS = C.pack . show

tasks :: (Show a) => ([Int] -> [a]) -> IO()
tasks mapFn = C.interact
    $ C.unlines . map showBS . mapFn . map readInt . drop 1 . C.words

---

prime :: Int
prime = 1000000007
 
modexp :: Int -> Int -> Int -> Int
modexp _ _ 0 = 1
modexp m b e | odd e     = b*r `mod` m
             | otherwise = r
  where r = modexp m (b*b `mod` m) (e `div` 2)

map3 :: (a -> a -> a -> b) -> [a] -> [b]
map3 f (x:y:z:xs) = f x y z : map3 f xs
map3 _ _          = []

main :: IO ()
main = tasks $ map3 (\a b c -> modexp prime a $ modexp (prime-1) b c)
