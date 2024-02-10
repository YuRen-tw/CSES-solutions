import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.List (unfoldr)

readInts :: C.ByteString -> [Int]
readInts = unfoldr (C.readInt . C.dropWhile isSpace)

showInt :: Int -> C.ByteString
showInt = C.pack . show

---

solve :: Int -> Int -> [Int] -> Int
solve s _    []     = s
solve s prev (x:xs) | x < prev  = solve (s + prev - x) prev xs
                    | otherwise = solve s x xs

main :: IO ()
main = C.interact $ showInt . solve 0 0 . drop 1 . readInts
