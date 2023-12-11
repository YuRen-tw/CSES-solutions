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
solve y x = lvl * (lvl-1) + sgn * (x-y) + 1
  where lvl = max x y
        sgn = lvl `mod` 2 * 2 - 1

main :: IO ()
main = tasks $ \(y:x:xs) -> (solve y x, xs)
