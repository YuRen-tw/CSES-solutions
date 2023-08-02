import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

showBS :: Show a => a -> C.ByteString
showBS = C.pack . show

---

solve :: Int -> Int -> [Int] -> Int
solve s _    []     = s
solve s prev (x:xs) | x < prev  = solve (s + prev - x) prev xs
                    | otherwise = solve s x xs

main :: IO ()
main = C.interact $ showBS . solve 0 0 . map readInt . drop 1 . C.words
