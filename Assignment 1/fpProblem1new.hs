countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b
	| a > b = 0
	| otherwise = length(findHappyNumbers (a+1) b [])

findHappyNumbers :: Int -> Int -> [Int] -> [Int]
findHappyNumbers a b ys
	| a > b = ys
	| elem a ys = findHappyNumbers (a+1) b ys
	| not (null happys) && head happys == a = findHappyNumbers (a+1) b (ys ++ happys)
	| otherwise = findHappyNumbers (a+1) b ys
	where happys = isHappy b [] ys a

isHappy :: Int -> [Int] -> [Int] -> Int -> [Int]
isHappy ub xs ys n
	| elem n ys = xs
	| rs == 1 = addElement 1 xs
	| n == 89 || elem n xs  = []
	| n > ub = isHappy ub xs ys rs
	| otherwise = isHappy ub (addElement n xs) ys rs
	where rs = resultDigits n


--findHappyNumbers :: Int -> Int -> [Int] -> [Int]
--findHappyNumbers

--isHappy :: Int -> [Int] -> [Int] -> Int -> [Int]
--isHappy



addElement :: Int -> [Int] -> [Int]
addElement n l
	| elem n l = l
	| otherwise = l ++ [n]

resultDigits :: Int -> Int
resultDigits n
	| n < 10 = n^2
	| otherwise = (mod n 10)^2 + resultDigits (div n 10)
