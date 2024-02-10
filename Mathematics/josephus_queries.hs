{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           System.IO (stdout)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as C
import           Data.Char (isSpace)
import           Data.List (unfoldr)

readInts :: C.ByteString -> [Int]
readInts = unfoldr (C.readInt . C.dropWhile isSpace)

buildInts :: [Int] -> B.Builder
buildInts = foldr (\x b -> B.intDec x <> "\n" <> b) mempty

tasks :: ([Int] -> Maybe (Int, [Int])) -> IO ()
tasks f = do
    _:ts <- readInts <$> C.getContents
    B.hPutBuilder stdout . buildInts $ unfoldr f ts

---

solve :: Int -> Int -> Int
solve 1 _ = 1
solve n k
    | k <= m            = 2 * k
    | odd n && nxt == 1 = n
    | odd n             = 2 * nxt - 3
    | otherwise         = 2 * nxt - 1
  where m   = n `div` 2
        nxt = solve (n-m) (k-m)

main :: IO ()
main = tasks $ \case
    n:k:xs -> Just (solve n k, xs)
    _      -> Nothing
