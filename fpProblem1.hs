countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b
	| a > b = 0
	| otherwise = findHappyNumbers (a+1) b []

findHappyNumbers :: Int -> Int -> [Int] -> Int
findHappyNumbers a b ys
	| a > b = length(ys)
	| isHappy [] ys a = findHappyNumbers (a+1) b (ys++[a])
	| otherwise = findHappyNumbers (a+1) b (ys)

isHappy :: [Int] -> [Int] -> Int -> Bool
isHappy xs ys n
	| elem n ys = True
	| resultDigits n == 1 = True
	| n == 89 || elem n xs  = False
	| otherwise = isHappy ([n]++ xs) ys (resultDigits n)

resultDigits :: Int -> Int
resultDigits n
	| n < 10 = n^2
	| otherwise = (mod n 10)^2 + resultDigits (div n 10)
