import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

showBS :: Show a => a -> C.ByteString
showBS = C.pack . show

---

level :: (Int, Int) -> (Int, Int)
level (lvl, n) | n > rng   = level (lvl + 1, n - rng)
               | otherwise = (lvl, n)
  where rng = lvl * 9 * 10 ^ (lvl - 1)

getdigit :: Int -> C.ByteString
getdigit pos = C.singleton $ C.index (showBS num) (intToInt64 idx)
  where (lvl, n) = level (1, pos - 1)
        (shf, idx) = n `quotRem` lvl
        num = 10 ^ (lvl-1) + shf
        intToInt64 = fromInteger . toInteger

main :: IO ()
main = C.interact $ C.unlines . map (getdigit . readInt) . drop 1 . C.lines
