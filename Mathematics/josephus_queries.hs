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

solve :: Int -> Int -> Int
solve 1 _ = 1
solve n k
    | k <= m            = 2 * k
    | odd n && nxt == 1 = n
    | odd n             = 2 * nxt - 3
    | otherwise         = 2 * nxt - 1
  where m   = n `div` 2
        nxt = solve (n-m) (k-m)

main :: IO ()
main = tasks $ \(n:k:xs) -> (solve n k, xs)
