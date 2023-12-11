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

cross :: (Int, Int) -> (Int, Int) -> Int
cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

solve :: Int -> Int -> Int -> Int -> Int -> Int -> C.ByteString
solve x1 y1 x2 y2 x3 y3 =
    case signum $ cross (x2-x1, y2-y1) (x3-x1, y3-y1) of
        0 -> C.pack "TOUCH"
        1 -> C.pack "LEFT"
        _ -> C.pack "RIGHT"

main :: IO ()
main = tasks
    $ \(x1:y1:x2:y2:x3:y3:xs) -> (solve x1 y1 x2 y2 x3 y3, xs)
