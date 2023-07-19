import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe (fromJust)

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

fromInt :: Int -> C.ByteString
fromInt = C.pack . show

---

map2 :: (a -> a -> b) -> [a] -> [b]
map2 f (x:y:xs) = f x y : map2 f xs
map2 _ _        = []

solve :: Int -> Int -> Int
solve y x = lvl * (lvl-1) + sgn * (x-y) + 1
  where lvl = max x y
        sgn = lvl `mod` 2 * 2 - 1

main :: IO ()
main = C.interact
    $ C.unlines . map fromInt . map2 solve . map readInt . drop 1 . C.words
