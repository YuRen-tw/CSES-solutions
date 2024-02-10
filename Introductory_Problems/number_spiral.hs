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
solve y x = lvl * (lvl-1) + sgn * (x-y) + 1
  where lvl = max x y
        sgn = lvl `mod` 2 * 2 - 1

main :: IO ()
main = tasks $ \case
    y:x:xs -> Just (solve y x, xs)
    _      -> Nothing
