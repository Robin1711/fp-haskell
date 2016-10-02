primes :: [Integer]
primes  =  sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)  =  p : sieve [x | x <- xs, x `mod` p /= 0]

-- Insert your own code here.
composites :: [(Integer,[Integer])]
composites = comp [4..] f
	where
		comp :: [Integer] -> (Integer -> [Integer]) -> [(Integer,[Integer])]
		comp (x:xs) f = (x,f x) : comp [x | x <- xs, not(isPrime x)] f

composition :: Integer -> [Integer]
composition n
	| n == 1 = []
	| otherwise = p : composition (div n p)
	where p = firstEl n primes

firstEl :: Integer -> [Integer] -> Integer
firstEl n (x:xs)
	| mod n x == 0 = x
	| otherwise = firstEl n xs

isPrime :: Integer -> Bool
isPrime n = n == head (dropWhile (<n) primes)

-- Do not change the following wrapper code
wrapper :: String -> [(Integer,[Integer])]
wrapper input = take (read input::Int) composites

main =  print . wrapper =<< getLine
