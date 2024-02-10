{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           System.IO (stdout)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
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

level :: (Int, Int, Int) -> (Int, Int, Int)
level (k, lvl, ten) | k > rng   = level (k - rng, lvl + 1, ten * 10)
                    | otherwise = (k, lvl, ten)
  where rng = lvl * 9 * ten

getDigit :: Int -> Int -> Int
getDigit n 1 = n `rem` 10
getDigit n i = getDigit (n `quot` 10) (i-1)

digitQuery :: Int -> Int
digitQuery k = getDigit num (lvl - idx)
  where (kk, lvl, ten) = level (k, 1, 1)
        (shf, idx) = (kk-1) `quotRem` lvl
        num = ten + shf

main :: IO ()
main = tasks $ \case
    k:ks -> Just (digitQuery k, ks)
    _    -> Nothing
