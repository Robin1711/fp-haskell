countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b
	| a > b = 0
	| otherwise = length(findHappyNumbers a b [])

findHappyNumbers :: Int -> Int -> [Int] -> [Int]
findHappyNumbers a b ys
	| a > b = ys
	| elem a ys = findHappyNumbers (a+1) b ys
	| not (null happys) && head happys == a = findHappyNumbers (a+1) b (addElements happys ys)
	| otherwise = findHappyNumbers (a+1) b ys
	where happys = isHappy b a [] ys

isHappy :: Int -> Int -> [Int] -> [Int] -> [Int]
isHappy ub n xs ys
	| elem n ys = xs
	| n == 1 = xs++[1]
	| n == 89 = []
	| n > ub = isHappy ub rsd xs ys
	| otherwise = isHappy ub rsd (xs++[n]) ys
	where rsd = resultDigits n

addElements :: [Int] -> [Int] -> [Int]
addElements [] l = l
addElements (x:xs) l
	| elem x l = addElements xs l
	| otherwise = (addElements xs l) ++ [x]

resultDigits :: Int -> Int
resultDigits n
	| n < 10 = n^2
	| otherwise = (mod n 10)^2 + resultDigits (div n 10)
