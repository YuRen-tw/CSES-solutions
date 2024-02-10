{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           System.IO (stdout)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.List (unfoldr)

readInts :: C.ByteString -> [Int]
readInts = unfoldr (C.readInt . C.dropWhile isSpace)

buildYN :: [Bool] -> B.Builder
buildYN = foldr f mempty
  where f x b = (if x then "YES\n" else "NO\n") <> b

tasks :: ([Int] -> Maybe (Bool, [Int])) -> IO ()
tasks f = do
    _:ts <- readInts <$> C.getContents
    B.hPutBuilder stdout . buildYN $ unfoldr f ts

---

solve :: Int -> Int -> Bool
solve a b = (a+b) `mod` 3 == 0 && 2 * min a b >= max a b

main :: IO ()
main = tasks $ \case
    a:b:xs -> Just (solve a b, xs)
    _      -> Nothing
