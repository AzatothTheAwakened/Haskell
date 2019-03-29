-- Exercise 1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

-- Exercise 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- Exercise 3
{-- (?) :: Int -> Int -> Int
1 ? _ = 1
x ? 1 = x
_ ? 0 = 1
x ? y = x * x ? (y - 1) --}

-- Exercise 4
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid (x - y) y
           | x < y = euclid (y - x) x
           | otherwise = 0

-- Exercise 5
ando :: [Bool] -> Bool
ando [] = True
ando (x:xs) = x && ando xs

{--concato :: [[a]] -> [a]
concato [[]] = []
concato (x:xs) = x : concato xs --}

{--replicateo :: Int -> a -> [a]
replicateo 0 x = []
replicateo n x = x : replicateo (n-1) x

(?) :: [a] -> Int -> a
(x:xs) ? 0 = x

(x:xs) ? n = xs ? (n-1)--}

elemo :: Eq a => a -> [a] -> Bool
elemo _ [] = False
elemo e (x:xs) = (x == e) || elemo e xs

-- Exercise 7
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs) | a <= b = a: merge as (b:bs)
                    | a >  b = b: merge (a:as) bs

-- Exercise 8
halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve x  = splitAt (div (length x) 2) x

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort (x:y:[]) = merge [x] [y]
msort x = (\(x,y) -> merge (msort x) (msort y)) (halve x)

-- Exercise 9
-- a)
sumo :: Integral a => [a] -> a
sumo [] = 0
sumo (x:xs) = x + sumo xs

-- b)
takeo :: Int -> [a] -> [a]
takeo 0 _ = []
takeo _ [] = []
takeo n (x:xs) = x: take (n-1) xs

-- c)
lasto :: [a] -> a
lasto (x:[]) = x
lasto (_:xs) = lasto xs 