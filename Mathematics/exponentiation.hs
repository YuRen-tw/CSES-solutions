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

-- The prime field mod 1000000007
newtype PrimeField = FieldElem { toInt :: Int }

fromInt :: Int -> PrimeField 
fromInt = FieldElem . (`mod` 1000000007)

instance Num PrimeField where
    (FieldElem a) + (FieldElem b) = fromInt (a + b)
    (FieldElem a) - (FieldElem b) = fromInt (a - b)
    (FieldElem a) * (FieldElem b) = fromInt (a * b)
    abs    = id
    signum = const 1
    fromInteger = fromInt . fromInteger

main :: IO ()
main = tasks $ \case
    a:b:xs -> Just (toInt $ FieldElem a ^ b, xs)
    _      -> Nothing
