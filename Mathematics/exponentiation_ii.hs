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

prime :: Int
prime = 1000000007
 
modexp :: Int -> Int -> Int -> Int
modexp _ _ 0 = 1
modexp m b e | odd e     = b*r `mod` m
             | otherwise = r
  where r = modexp m (b*b `mod` m) (e `div` 2)

main :: IO ()
main = tasks $ \(a:b:c:xs)-> (modexp prime a $ modexp (prime-1) b c, xs)
