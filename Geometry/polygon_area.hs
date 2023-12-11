chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f xs = y : chop f ys
  where (y, ys) = f xs

cyc :: (a -> a -> b) -> [a] -> [b]
cyc _ []  = []
cyc _ [_] = []
cyc f (x0:x1:xs) = f x0 x1 : cyc' f x0 x1 xs
  where cyc' g p0 p1 []      = [g p1 p0]
        cyc' g p0 p1 (p2:ps) = g p1 p2 : cyc' g p0 p2 ps

---

cross :: (Int, Int) -> (Int, Int) -> Int
cross (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

area :: [Int] -> Int
area = abs . sum . cyc cross . chop (\(x:y:xs) -> ((x,y), xs))

main :: IO ()
main = interact $ show . area . map read . drop 1 . words
