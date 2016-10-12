fibcats :: [Integer]
-- Insert your own code here.
fibcats  =  sieve [0..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)  =  p : sieve [x | x <- xs, (isFib x || isCat x)]

--fibcats :: [Integer]
--fibcats = merge fibonacci catalan []

--merge :: [Integer] -> [Integer] -> [Integer] -> [Integer]
--merge (x:xs) (y:ys) zs
--	| x == y && not(elem x zs) = x : merge xs ys ([x]++zs)
--	| x < y && not(elem x zs) = x : merge xs (y:ys) ([x]++zs)
--	| y < x && not(elem y zs) = y : merge (x:xs) ys ([y]++zs)
--	| otherwise = merge xs ys zs

fibonacci :: [Integer]
fibonacci = 0 : calc [0,1]
	where
		calc :: [Integer] -> [Integer]
		calc (x:xs) = lst : calc [lst, (x + lst)]
			where lst = last xs

isFib :: Integer -> Bool
isFib n = n == head (dropWhile (<n) fibonacci)

catalan :: [Integer]
catalan = calc 0
	where
		calc :: Integer -> [Integer]
		calc n = next n : calc (n+1)

next :: Integer -> Integer
next n = div (fact (2*n)) ((fact n) * (fact (n+1)))

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

isCat :: Integer -> Bool
isCat n = n == head (dropWhile (<n) catalan)

-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) fibcats

main =  print . wrapper =<< getLine
