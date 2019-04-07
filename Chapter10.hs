import System.IO
import System.Console.ANSI

-- HANGMAN

hangman :: IO ()
hangman = do putStrLn "Think of a Word"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh =  do hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!!"
               else
                  do putStrLn (match word guess)
                     play word


match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- Exercise 1
adder' :: Int -> Int -> IO Int
adder' akk 0 = return akk
adder' akk n = do line <- getLine
                  adder' (read line +akk) (n-1)

adder :: IO ()
adder = do putStrLn "How many numbers?"
           line <- getLine
           x <- adder' 0 (read line)
           putStr "The total is "
           print x
