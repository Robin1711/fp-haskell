isCorrectTakuzu :: [String] -> Integer
isCorrectTakuzu xs = fill (getStrings (getRow 1 xs) (takuzuStrings (lengthOf xs))) 1 (lengthOf xs) xs

lengthOf :: [a] -> Integer
lengthOf [] = 0
lengthOf (x:xs) = 1 + lengthOf xs

--(takuzuStrings (length s))
--function to add all correct solutions

{-fillRow :: [String] -> [String] -> Int -> Int -> [String] --input: takuzu, possible input, row for input, number of rows
fillRow xs input n end
	| n == end = newTakuzu (head input) xs n
	| otherwise = fillRow (newTakuzu (head input) xs n) getStrings  (n+1)
-}

fill :: [String] -> Integer -> Integer -> [String] -> Integer
fill xs n end ts
	| (xs == []) && (n==end) && correctSolution ts = 1
	| (xs == []) && (n==end) && not(correctSolution ts) = 0
	| xs == [] = fill (getStrings (getRow (n+1) ts) (takuzuStrings end)) (n+1) end ts
--	| xs == [] = 0
	| n == end = fill (tail xs) n end (newTakuzu (head xs) ts n)
	| otherwise = fill (getStrings (getRow (n+1) ts) (takuzuStrings end)) (n+1) end (newTakuzu (head xs) ts n) + fill (tail xs) n end (newTakuzu (head xs) ts n)
	
getRow :: Integer -> [String] -> String
getRow 1 xs = head xs
getRow n xs = getRow (n-1) (tail xs)

--function to get takuzu for every input
	
newTakuzu :: String -> [String] -> Integer -> [String] -- put string s on row within takuzu (first row is 1)
newTakuzu s (x:xs) row
	| row == 1 = [s] ++ xs
	| otherwise = [x] ++ newTakuzu s xs (row-1)

getStrings :: String -> [String] -> [String] --get a row or column from Takuzu and return all possible answers
getStrings s strings
	| strings == [] 					= []
	| correctAnswer s (head strings) 	= getStrings s (tail strings) ++ [(head strings)]
	| otherwise							= getStrings s (tail strings)


correctAnswer :: String -> String -> Bool -- string s is a string of the takuzu, string t returns true if it is an answer to s
correctAnswer s t
	| (s == "") && (t == "")	= True
	| (head s) == (head t)		= correctAnswer (tail s) (tail t)
	| (head s) == '.'			= correctAnswer (tail s) (tail t)
	| otherwise 				= False


-- following code determines if answer to filled in takuzu is correct
correctSolution :: [String] -> Bool
correctSolution xs = not(sameColumn xs) && not(sameRow xs) && (columnsCorrect xs)

columnsCorrect :: [String] -> Bool --return true if all columns are takuzustrings
columnsCorrect [] = True
columnsCorrect xs
	| columnIsTakuzu (getFirstColumn xs) = columnsCorrect (removeFirstColumn xs)
	| otherwise = False

columnIsTakuzu :: String -> Bool
columnIsTakuzu s = (checkString s ' ' 0) && (sameNumber s 0 0)

sameColumn :: [String] -> Bool -- return true if there are any same columns
sameColumn [] = False -- no same columns
sameColumn xs
	| columns (getFirstColumn xs) (removeFirstColumn xs) = True --not a correct solution
	| otherwise = sameColumn (removeFirstColumn xs)
	
getFirstColumn :: [String] -> String
getFirstColumn [] = ""
getFirstColumn (x:xs) = [(head x)] ++ getFirstColumn xs

columns :: String -> [String] -> Bool --return true if given column is same as one of the other columns
columns s [] = False
columns s xs
	| column s xs = True
	| otherwise = columns s (removeFirstColumn xs)
	
removeFirstColumn :: [String] -> [String]
removeFirstColumn [] = []
removeFirstColumn (x:xs) 
	| (tail x) == "" = []
	| otherwise = [(tail x)] ++ removeFirstColumn xs

column :: String -> [String] -> Bool -- return true if given column is same as the first column
column s [] = True
column s (x:xs)
	| (head s) == (head x) = column (tail s) xs
	| otherwise = False

sameRow :: [String] -> Bool
sameRow [] = False -- no same rows
sameRow (x:xs)
	| rows x xs = True --not a correct solution
	| otherwise = sameRow xs

rows :: String -> [String] -> Bool --return true if given row is same as one of the other rows
rows s [] = False
rows s (x:xs)
	| s == x	= True
	| otherwise = rows s xs
	
	


-- code problem 2 plus added function sameNumber
takuzuStrings :: Integer -> [String]
takuzuStrings n = checkAll (reverse (produceStrings n ((2^n)-1)))

produceStrings :: Integer -> Integer -> [String]
produceStrings n i
	| i == 0 = [strings i n]
	| otherwise = [strings i n] ++ produceStrings n (i-1)

strings :: Integer -> Integer -> String
strings i n
	| n==0 = ""
	| i - (2^(n-1)) >= 0 = "1" ++ strings (i-2^(n-1)) (n-1)
	| otherwise = "0" ++ strings i (n-1)

checkAll :: [String] -> [String]
checkAll [] = []
checkAll (x : xs)
	| (checkString x ' ' 0) && (sameNumber x 0 0) = [x] ++ checkAll xs
	| otherwise = checkAll xs

checkString :: String -> Char -> Integer -> Bool
checkString s l amount
	| (head s) == l && amount == 2 = False
	| tail s == "" = True
	| head s == l = checkString (tail s) l (amount+1)
	| otherwise = checkString (tail s) (head s) 1


--this function returns true if a string has same number of zeroes and ones
sameNumber :: String -> Int -> Int -> Bool
sameNumber s zero one
	| (s == "") && zero==one = True
	| (s == "") && zero/=one = False
	| (head s) == '0' = sameNumber (tail s) (zero+1) one
	| (head s) == '1' = sameNumber (tail s) zero (one+1)





--main =  print . isCorrectTakuzu .lines =<< getContents
