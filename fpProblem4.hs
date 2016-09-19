lastDigits :: Integer -> Integer -> [Integer]
lastDigits n d
	| (n>0) && (digits s <= digits d) = putInList s (digits s)
	| (n>0) && (digits s > digits d) = putInList s d
	where s = sumNum2 n n 0 d

sumNum :: Integer -> Integer -> Integer
sumNum num pow
	| pow ==  0 = 1
	| otherwise = num^pow + sumNum num (pow-1)
	
	
sumNum2 :: Integer -> Integer -> Integer -> Integer -> Integer
sumNum2 num pow n d
	| numberOfZeroes (num^n) > d	= 0
	| n ==  pow = num^n
	| otherwise = num^n + sumNum2 num pow (n+1) d
	

numberOfZeroes :: Integer -> Integer
numberOfZeroes n
	| mod n 10 /= 0 	= 0
	| otherwise = 1 + numberOfZeroes (div n 10)

putInList :: Integer -> Integer -> [Integer]
putInList s d
	| d == 1 = [mod s 10]
	| mod s 10 == s = [s]
	| otherwise = putInList (div s 10) (d-1) ++ [(mod s 10)]

digits :: Integer -> Integer
digits n
	| mod n 10 == n = 1
	| otherwise = 1 + digits (div n 10)
