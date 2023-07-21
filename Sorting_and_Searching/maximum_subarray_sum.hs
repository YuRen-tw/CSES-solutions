import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)
 
readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt
 
showBS :: Show a => a -> C.ByteString
showBS = C.pack . show
 
---

-- Kadane's algorithm

lb :: Int
lb = -1000000000

mssStep :: Int -> (Int, Int) -> (Int, Int)
mssStep x (best, prev) = (max best curr, curr)
  where curr = max x (prev + x)

mss :: [Int] -> Int
mss = fst . foldr mssStep (lb, 0)

main :: IO ()
main = C.interact $ showBS . mss . map readInt . drop 1 . C.words
