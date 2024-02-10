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

prime :: Int
prime = 1000000007
 
modexp :: Int -> Int -> Int -> Int
modexp _ _ 0 = 1
modexp m b e | odd e     = b*r `mod` m
             | otherwise = r
  where r = modexp m (b*b `mod` m) (e `div` 2)

main :: IO ()
main = tasks $ \case
    a:b:c:xs -> Just (modexp prime a $ modexp (prime-1) b c, xs)
    _        -> Nothing
