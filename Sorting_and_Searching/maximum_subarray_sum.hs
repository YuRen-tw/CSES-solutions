import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.List (unfoldr)

readInts :: C.ByteString -> [Int]
readInts = unfoldr (C.readInt . C.dropWhile isSpace)

showInt :: Int -> C.ByteString
showInt = C.pack . show

---

-- Kadane's algorithm
kadane :: Int -> (Int, Int) -> (Int, Int)
kadane x (best, prev) = (max best curr, curr)
  where curr = max x (prev + x)

mss :: [Int] -> Int
mss = fst . foldr kadane (lb, 0)
  where lb = -1000000000

main :: IO ()
main = C.interact $ showInt . mss . drop 1 . readInts
