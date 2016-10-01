primes :: [Integer]
primes  =  2 : sieve [1..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p:xs)  =  (2*p)+1 : sieve [x| x <- xs, not(isRemoval x)]

removals :: [Integer]
removals = calc [1..]
	where
		calc :: [Integer] -> [Integer]
		calc (j:xs) = [i + j + 2*i*j | i <- [1..j]] ++ calc xs

isRemoval :: Integer -> Bool
isRemoval n = elem n (take (sum [1..j]) removals)
			where j = fromInteger ((div n 3) + 1)

-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes
main =  print . wrapper =<< getLine
