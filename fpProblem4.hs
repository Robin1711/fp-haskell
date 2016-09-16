lastDigits :: Int -> Int -> [Int]
lastDigits n d
	| digits s < digits d = putInList s (digits s)
	| otherwise = putInList s d
	where s = sumNum n n

sumNum :: Int -> Int -> Int
sumNum num pow
	| pow ==  0 = 1
	| otherwise = num^pow + sumNum num (pow-1)

putInList :: Int -> Int -> [Int]
putInList s d
	| d == 1 = [mod s 10]
	| mod s 10 == s = [s]
	| otherwise = putInList (div s 10) (d-1) ++ [(mod s 10)]

digits :: Int -> Int
digits n
	| mod n 10 == n = 1
	| otherwise = 1 + digits (div n 10)
