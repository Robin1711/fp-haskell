countHappyNumbers :: Integer -> Integer -> Int
countHappyNumbers a b
	| a > b	 = 0
	| otherwise = findHappyNumbers a b
	
findHappyNumbers :: Integer -> Integer -> Int
findHappyNumbers a b
	| a == b && isHappy a 	= 1
	| a==b 					= 0
	| isHappy a 			= 1 + findHappyNumbers (a+1) b
	| otherwise 			= findHappyNumbers (a+1) b

isHappy :: Integer -> Bool
isHappy a
	| a == 1 	= True
	| a == 4	= False
	| otherwise = isHappy (resultDigits a)
	
resultDigits :: Integer -> Integer
resultDigits n
	| n < 10 = n^2
	| otherwise = (mod n 10)^2 + resultDigits (div n 10)
	

wrapper :: [String] -> Int
wrapper (a:b:_) = countHappyNumbers (read a::Integer) (read b::Integer)

main =  print . wrapper . words =<< getLine
