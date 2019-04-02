-- Exercise 1
reexpr :: (a -> a) -> (a -> Bool) -> [a] -> [a]
reexpr f p xs = map f (filter (p) xs)

-- Exercise 2
allo :: (a -> Bool) -> [a] -> Bool
allo p = and . map (p)

anyo :: (a -> Bool) -> [a] -> Bool
anyo p = and . map (p) 

takeWhileo :: (a -> Bool) -> [a] -> [a]
takeWhileo p [] = []
takeWhileo p (x:xs) | p x = x : takeWhileo (p) xs
                    | otherwise = []

dropWhileo :: (a -> Bool) -> [a] -> [a]
dropWhileo p [] = []
dropWhileo p (x:xs) | p x = dropWhile (p) xs
                    | otherwise = x:xs
-- Exercise 3
mapfr :: (a -> b) -> [a] -> [b]
mapfr f [] = []
mapfr f xs = foldr (\x y -> f x : y) [] xs

filterfr' :: (a -> Bool) -> a -> [a]
filterfr' p x | p x = [x]
              | otherwise = []

filterfr :: (a -> Bool) -> [a] -> [a]
filterfr p = foldr (\x y -> (filterfr' (p) x) ++ y ) []

-- Exercise 4
dec2int :: [Int] -> Int
dec2int xs = foldl (\x y -> x*10 + y) 0 xs

-- Exercise 5
curryo :: ((a,b) -> c) -> a -> b -> c
curryo f x y = f (x, y)

uncurryo :: (a -> b -> c) -> (a,b) -> c
uncurryo f = \(x,y) -> f x y

-- Exercise 6
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 xs = unfold (\x -> length x == 0) (take 8) (drop 8) xs

mapo :: (a -> b) -> [a] -> [b]
mapo f = unfold (null) (f . head) (drop 1)

iterateo :: (a -> a) -> a -> [a]
iterateo f x = unfold (\x -> True) (f . head ) (id) [x]

-- Exercise 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs
