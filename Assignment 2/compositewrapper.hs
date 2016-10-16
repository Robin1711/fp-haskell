primes :: [Integer]
primes  =  sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)  =  p : sieve [x | x <- xs, x `mod` p /= 0]

-- Insert your own code here.
composites :: [(Integer,[Integer])]
composites = comp [4..] composition
	where
		comp :: [Integer] -> (Integer -> [Integer]) -> [(Integer,[Integer])]
		comp (x:xs) f = (x,f x) : comp [x | x <- xs, not(isPrime x)] f

-- Calculate the prima factorization of the given integer
composition :: Integer -> [Integer]
composition n
	| n == 1 = []
	| otherwise = p : composition (n `div` p)
	where p = divider n primes

divider :: Integer -> [Integer] -> Integer
divider n (x:xs)
	| n `rem` x == 0 = x
	| otherwise = divider n xs

isPrime :: Integer -> Bool
isPrime n = n == head (dropWhile (<n) primes)

-- Do not change the following wrapper code
wrapper :: String -> [(Integer,[Integer])]
wrapper input = take (read input::Int) composites

main =  print . wrapper =<< getLine
