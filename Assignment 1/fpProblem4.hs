lastDigits :: Integer -> Int -> [Integer]
lastDigits n d
	| (n>0) && (digits s <= d) && larger n 0 d 		= putInList s d --if we have had a number with more than d digits, we need to print zeroes in front of s
	| (n>0) && (digits s <= d) && not(larger n 0 d) 	= putInList s (digits s)
	| (n>0) && (digits s > d) 						= putInList s d
	where s = sumNum n n 0 d

	
sumNum :: Integer -> Integer -> Integer -> Int -> Integer --num is only d digits long every time
sumNum num pow n d
	| n == pow 	= mod (num*pow) digit
	| n == 0 	= (mod (num^n) digit) + sumNum (mod (num^n) digit) pow (n+1) d
	| otherwise = (mod (num*pow) digit) + sumNum (mod (num*pow) digit) pow (n+1) d
	where digit = 10^d
	
-- this function finds out if the number computed is larger than the amount of digits we need to print
larger :: Integer -> Integer -> Int -> Bool
larger n pow d 
	| n == pow && n^pow>(10^d) = True
	| n == pow  = False
	| n^pow>(10^d) = True
	| otherwise = larger n (pow+1) d
	
putInList :: Integer -> Int -> [Integer]
putInList s d
	| d == 1 		= [mod s 10]
	| digits s < d 	= [0] ++ putInList s (d-1)
	| mod s 10 == s = [s]
	| otherwise 	= putInList (div s 10) (d-1) ++ [(mod s 10)]

digits :: Integer -> Int
digits n
	| mod n 10 == n = 1
	| otherwise = 1 + digits (div n 10)
	
wrapper :: [String] -> [Integer]
wrapper (a:b:_) = lastDigits (read a::Integer) (read b::Int)

main =  print . wrapper . words =<< getLine

