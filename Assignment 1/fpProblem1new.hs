countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b
	| a > b = 0
	| otherwise = length(removeUnwanted a b happys)
	where happys = findHappyNumbers a b []

findHappyNumbers :: Int -> Int -> [Int] -> [Int]
findHappyNumbers a b ys
	| a > b = ys
	| elem a ys = findHappyNumbers (a+1) b ys
	| not (null happys) && head happys == a = findHappyNumbers (a+1) b (ys++happys)
	| otherwise = findHappyNumbers (a+1) b ys
	where happys = isHappy b a [] ys

isHappy :: Int -> Int -> [Int] -> [Int] -> [Int]
isHappy ub n xs ys
	| n == 1 = xs++[1]
	| n == 89 = []
	| elem n ys = xs
	| otherwise = isHappy ub rsd (xs++[n]) ys
	where rsd = resultDigits n

removeUnwanted :: Int -> Int -> [Int] -> [Int]
removeUnwanted _ _ [] = []
removeUnwanted lb ub (x:xs)
	|  x > ub || x < lb = removeUnwanted lb ub xs
	| elem x xs = removeUnwanted lb ub xs
	| otherwise = [x] ++ removeUnwanted lb ub xs

resultDigits :: Int -> Int
resultDigits n
	| n < 10 = n^2
	| otherwise = (mod n 10)^2 + resultDigits (div n 10)
