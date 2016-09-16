lastDigits :: Int -> Int -> [Int]
lastDigits n d
	| digits s < digits d = putInList s
	| otherwise = putInList (getCorrectDigitsOfSum d s)
	where s = sumNum n n 0

sumNum :: Int -> Int -> Int -> Int
sumNum num pow res
	| pow < 0  = res
	| otherwise = sumNum num (pow-1) (res + num^pow)

putInList :: Int -> [Int]
putInList s
	| mod s 10 == s = [s]
	| otherwise = putInList (div s 10) ++ [(mod s 10)]

getCorrectDigitsOfSum :: Int -> Int -> Int
getCorrectDigitsOfSum d num
	| digits num == d = num
	| otherwise = getCorrectDigitsOfSum d (removeFirstDigit num)

digits :: Int -> Int
digits n
	| mod n 10 == n = 1
	| otherwise = 1 + digits (div n 10)

removeFirstDigit :: Int -> Int
removeFirstDigit num = num - ((getFirstDigit num ) * (10^((digits num)-1)))

getFirstDigit :: Int -> Int
getFirstDigit n
	| mod n 10 == n = n
	| otherwise = getFirstDigit (div n 10)