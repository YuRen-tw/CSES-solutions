{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           System.IO (stdout)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as C
import           Data.Char (isSpace)
import           Data.List (unfoldr)

(<>) = mappend

readInts :: C.ByteString -> [Int]
readInts = unfoldr (C.readInt . C.dropWhile isSpace)

buildSigns :: [Int] -> B.Builder
buildSigns = foldr f mempty
  where f 0 b = "TOUCH\n" <> b
        f 1 b = "LEFT\n" <> b
        f _ b = "RIGHT\n" <> b

tasks :: ([Int] -> Maybe (Int, [Int])) -> IO ()
tasks f = do
    _:ts <- readInts <$> C.getContents
    B.hPutBuilder stdout . buildSigns $ unfoldr f ts

---

cross :: (Int, Int) -> (Int, Int) -> Int
cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Int
solve x1 y1 x2 y2 x3 y3 = signum $ cross (x2-x1, y2-y1) (x3-x1, y3-y1)

main :: IO ()
main = tasks $ \case
    x1:y1:x2:y2:x3:y3:xs -> Just (solve x1 y1 x2 y2 x3 y3, xs)
    _                    -> Nothing
