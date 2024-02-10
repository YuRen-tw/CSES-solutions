import qualified Data.ByteString.Char8   as C
import           Data.Char (isSpace)
import           Data.List (unfoldr)

readInts :: C.ByteString -> [Int]
readInts = unfoldr (C.readInt . C.dropWhile isSpace)

showInt :: Int -> C.ByteString
showInt = C.pack . show

---

solve :: [Int] -> Int
solve = g . foldr f (0, 0)
  where f t (total, longest) = (total + t, max longest t)
        g (total, longest) = max total (longest * 2)

main :: IO ()
main = C.interact $ showInt . solve . drop 1 . readInts
