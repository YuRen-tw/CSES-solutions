import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.List (unfoldr)

readInts :: C.ByteString -> [Int]
readInts = unfoldr (C.readInt . C.dropWhile isSpace)

showInt :: Int -> C.ByteString
showInt = C.pack . show

---

miss :: [Int] -> Int
miss xs = tri_num (head xs) - sum (tail xs)
  where tri_num n = n * (n+1) `div` 2

main :: IO ()
main = C.interact $ showInt . miss . readInts
