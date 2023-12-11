import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f xs = y : chop f ys
  where (y, ys) = f xs

tasks :: ([Int] -> (C.ByteString, [Int])) -> IO()
tasks f = C.interact
    $ C.unlines . chop f . map readInt . drop 1 . C.words

---

solve :: Int -> Int -> C.ByteString
solve a b =
    if (a+b) `mod` 3 == 0 && 2 * min a b >= max a b
    then C.pack "YES"
    else C.pack "NO"

main :: IO ()
main = tasks $ \(a:b:xs) -> (solve a b, xs)
