-- Exercise one
halve :: [a] -> ([a], [a])
halve a = (take ((length a) `div` 2) a,drop ((length a) `div` 2) a)

-- Exercise two
thirda :: [a] -> a
thirda a = head $ tail $ tail a

thirdb :: [a] -> a
thirdb a = a !! 2

thirdc :: [a] -> a
thirdc (_:(_:(a:_))) = a

--Exercise 3
safetaila :: [a] -> [a]
safetaila a = if length a == 0 
                then [] 
                else tail a

safetailb :: [a] -> [a]
safetailb a | length a == 0 = []
            | otherwise = tail a

safetailc :: [a] -> [a]
safetailc [] = []
safetailc a = tail a

-- Exercise 4
-- Commented out because ambiguous
{-
(?) :: Bool -> Bool -> Bool
True  ? True  = True
True  ? False = True
False ? False = True
False ? True  = True
-}
{-
(?) :: Bool -> Bool -> Bool
True ? _  = True
_ ? True  = True
False ? _ = False
-}
{-
(?) :: Bool -> Bool -> Bool
False ? False = False
_ ? _         = True
-}
{-}
(?) :: Bool -> Bool -> Bool
False ? b = b
True ? _ = True
-}

-- Exercise 5
and' :: Bool -> Bool -> Bool
and' a b = if a == True 
                then if b == True 
                    then True
                    else False
                else False

-- Exercise 6
and'' :: Bool -> Bool -> Bool
and'' a b = if a == True 
                then b
                else False

-- Exercise 7
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- Exercise 8
luhnDouble :: Int -> Int
luhnDouble x =  if x*2 > 9 
                    then x*2-9 
                    else x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (mod (sum (map luhnDouble [a,c]) + b + d) 10) == 0