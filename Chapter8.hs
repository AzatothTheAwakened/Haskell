-- Exercise 1
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul n m = iterate (add m) m !! (nat2int n-1)

ex1tst :: Int
ex1tst = nat2int (mul (int2nat 9) (int2nat 7))

-- Exercise 2
data Tree a = Leaf a | Node (Tree a) (Tree a)

countleafs :: Tree a -> Int
countleafs (Leaf _) = 1
countleafs (Node a b) = countleafs a + countleafs b

balanced :: Tree a -> Bool
balanced (Node x y) = countleafs x == countleafs y

-- Exercise 3
split2 :: [a] -> ([a],[a])
split2 xs = splitAt (div (length xs) 2) xs

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
             where l = fst (split2 xs)
                   r = snd (split2 xs)
