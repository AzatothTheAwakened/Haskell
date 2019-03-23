-- Exercise 1
sumSquaresTo100 :: Int
sumSquaresTo100 = sum [x^2 | x <- [1..100]]

-- Exercise 2
grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

-- Exercise 3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Exercise 4
replicateO :: Int -> a -> [a]
replicateO n m = [m | _ <- [1..n]]

-- Exercise 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x^2 + y^2) == z^2]

-- Exercise 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], (sum $ factors x) - x == x]

-- Exercise 7
bsp :: [(Int,Int)]
bsp = [(x,y) | x <- [1,2], y <- [3,4]]

exc7 :: [(Int,Int)]
exc7 = concat [[(1,x) | x <- [1,2]], [(2,x) | x <- [3,4]]]

-- Exercise 8
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x ==x'] 

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positionso :: Eq a => a -> [a] -> [Int]
positionso x xs = find x [(k,v) | (k, v) <- zip xs [0..]]

-- Exercise 9
scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [x * y | (x,y) <- zip xs ys]
 