countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b
	| a > b = 0
	| otherwise = countHappyNumbers (a+1) b + isHappy [] a

isHappy :: [Int] -> Int -> Int
isHappy xs n
	| resultDigits n == 1 = 1 		--True
	| elem n xs = 0					--False
	| otherwise = isHappy ([n]++ xs) (resultDigits n)

resultDigits :: Int -> Int
resultDigits n
	| n < 10 = n^2
	| otherwise = (mod n 10)^2 + resultDigits (div n 10)

