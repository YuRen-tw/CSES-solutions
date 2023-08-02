import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

tasks :: ([Int] -> [C.ByteString]) -> IO()
tasks mapFn = C.interact
    $ C.unlines . mapFn . map readInt . drop 1 . C.words

---

solve :: Int -> Int -> C.ByteString
solve a b =
    if (a+b) `mod` 3 == 0 && 2 * min a b >= max a b
    then C.pack "YES"
    else C.pack "NO"

map2 :: (a -> a -> b) -> [a] -> [b]
map2 f (x:y:xs) = f x y : map2 f xs
map2 _ _        = []

main :: IO ()
main = tasks $ map2 solve
