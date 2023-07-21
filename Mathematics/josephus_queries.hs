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

solve :: Int -> Int -> Int
solve 1 _ = 1
solve n k
    | k <= m            = 2 * k
    | odd n && nxt == 1 = n
    | odd n             = 2 * nxt - 3
    | otherwise         = 2 * nxt - 1
  where m   = n `div` 2
        nxt = solve (n-m) (k-m)

map2 :: (a -> a -> b) -> [a] -> [b]
map2 f (x:y:xs) = f x y : map2 f xs
map2 _ _        = []

main :: IO ()
main = tasks $ map2 solve
