lastDigits :: Int -> Int -> [Int]
lastDigits n d
	| digits s < digits d = putInList s (digits s)
	| otherwise = putInList s d
	where s = sumNum n n 0

sumNum :: Int -> Int -> Int -> Int
sumNum num pow res
	| pow < 0  = res
	| otherwise = sumNum num (pow-1) (res + num^pow)

putInList :: Int -> Int -> [Int]
putInList s d
	| d == 1 = [d]
	| mod s 10 == s = [s]
	| otherwise = putInList (div s 10) (d-1) ++ [(mod s 10)]

digits :: Int -> Int
digits n
	| mod n 10 == n = 1
	| otherwise = 1 + digits (div n 10)
