lastDigits :: Integer -> Int -> [Integer]
lastDigits n d
	| (n>0) && (digits s <= d) = putInList s (digits s)
	| (n>0) && (digits s > d) = putInList s d
	where s = sumNum n n 0 d

sumNum :: Integer -> Integer -> Integer -> Int -> Integer
sumNum num pow n d
	| numberOfZeroes (num^n) > d	= 0
	| n ==  pow = num^n
	| otherwise = num^n + sumNum num pow (n+1) d
	
numberOfZeroes :: Integer -> Int
numberOfZeroes n
	| mod n 10 /= 0 	= 0
	| otherwise 		= 1 + numberOfZeroes (div n 10)

putInList :: Integer -> Int -> [Integer]
putInList s d
	| d == 1 = [mod s 10]
	| mod s 10 == s = [s]
	| otherwise = putInList (div s 10) (d-1) ++ [(mod s 10)]

digits :: Integer -> Int
digits n
	| mod n 10 == n = 1
	| otherwise = 1 + digits (div n 10)

wrapper :: [String] -> [Integer]
wrapper (a:b:_) = lastDigits (read a::Integer) (read b::Int)

main =  print . wrapper . words =<< getLine

