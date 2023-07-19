import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

fromInt :: Int -> C.ByteString
fromInt = C.pack . show

---

miss :: [Int] -> Int
miss xs = tri_num (head xs) - sum (tail xs)
  where tri_num n = n * (n+1) `div` 2

main :: IO ()
main = C.interact $ fromInt . miss . map readInt . C.words
