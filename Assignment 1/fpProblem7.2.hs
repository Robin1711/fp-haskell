isCorrectTakuzu :: [String] -> Bool
isCorrectTakuzu xs  
	|noDots (change xs) = correctSolution (change xs) --if there are no dots left in the takuzu, it is completely filled in
	|otherwise = (checks xs == 1)

lengthOf :: [a] -> Integer
lengthOf [] = 0
lengthOf (x:xs) = 1 + lengthOf xs

noDots :: [String] -> Bool
noDots [] = True
noDots (x:xs)
	| noDotsInString x 	= noDots xs
	| otherwise			= False

noDotsInString :: String -> Bool
noDotsInString [] = True
noDotsInString s
	| (head s) == '.' 	= False
	| otherwise 		= noDotsInString (tail s)

	
-- check how many different answers there are for the takuzu
checks :: [String] -> Integer
checks xs = lengthOf(filled) - numberOfSame (filled)
	where filled = fill (getStrings (getRow 1 xs) (takuzuStrings (lengthOf xs))) 1 (lengthOf xs) (change xs)

-- function gives back number of same lists in the list.
numberOfSame :: [[String]] -> Integer
numberOfSame [] = 0
numberOfSame (x:xs)
	| sameList x xs = 1 + numberOfSame xs
	| otherwise = numberOfSame xs
	
sameList :: [String] -> [[String]] -> Bool --return true if given list is same as one of the other lists
sameList xs [] = False
sameList xs xss
	| xs == (head xss)	= True
	| otherwise = sameList xs (tail xss)

-- fill takuzu with computed takuzuStrings and check if they give a correct solution
fill :: [String] -> Integer -> Integer -> [String] -> [[String]]
fill xs n end ts
	| (xs == []) && (n==end) && correctSolution ts = [ts]
	| (xs == []) && (n==end) && not(correctSolution ts) = []
	| xs == [] = fill (getStrings (getRow (n+1) ts) (takuzuStrings end)) (n+1) end ts
	| n == end = fill (tail xs) n end (newTakuzu (head xs) ts n)
	| otherwise = fill (getStrings (getRow (n+1) ts) (takuzuStrings end)) (n+1) end (newTakuzu (head xs) ts n) ++ fill (tail xs) n end (newTakuzu (head xs) ts n)
	
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
produceStrings n i --where n is the length of the string and i is the number the bitstring represents
	| i == 0 	= [strings n i]
	| otherwise = [strings n i] ++ produceStrings n (i-1)

--convert number i into a bitstring
strings :: Integer -> Integer -> String
strings n i 
	| n == 0 				= ""
	| i - (2^(n-1)) >= 0 	= "1" ++ strings (n-1) (i-2^(n-1)) 
	| otherwise 			= "0" ++ strings (n-1) i 

checkAll :: [String] -> [String]
checkAll [] = []
checkAll (x : xs)
	| (checkString x ' ' 0) && (sameNumber x 0 0) 	= [x] ++ checkAll xs
	| otherwise 									= checkAll xs

--check if there are no more than 2 ones or zeros in a row
checkString :: String -> Char -> Integer -> Bool
checkString s c amount
	| (head s) == c && amount == 2 	= False
	| tail s == "" 					= True
	| head s == c 					= checkString (tail s) c (amount+1)
	| otherwise 					= checkString (tail s) (head s) 1 


--this function returns true if a string has same number of zeroes and ones
sameNumber :: String -> Int -> Int -> Bool
sameNumber s zero one
	| (s == "") && zero==one = True
	| (s == "") && zero/=one = False
	| (head s) == '0' = sameNumber (tail s) (zero+1) one
	| (head s) == '1' = sameNumber (tail s) zero (one+1)




-- code to fill takuzu on the basis of 2 of the same numbers in a row, 2 of the same numbers and a dot between them
--and that there should be the same amount of zeroes and ones in a row


--fill rows and columns untill nothing changes anymore
change :: [String] -> [String]
change xs
	| (diagonal (fillKnownColumns xs) == xs) && (fillKnownRows xs == xs) = xs
	| diagonal (fillKnownColumns xs) == xs = change (fillKnownRows xs)
	| fillKnownRows xs == xs = change (diagonal (fillKnownColumns xs))
	| otherwise  = change (diagonal (fillKnownColumns (fillKnownRows xs)))

--fill in everything you know for sure
fillKnownRows :: [String] -> [String]
fillKnownRows [] = []
fillKnownRows (x:xs) = [maximumAmount(between(reverse(double(reverse(double x)))))] ++ fillKnownRows xs 

fillKnownColumns :: [String] -> [String]
fillKnownColumns [] = []
fillKnownColumns xs = [maximumAmount(between(reverse(double(reverse(double(getFirstColumn xs))))))] ++ fillKnownColumns (removeFirstColumn xs)

-- switch columns to rows
diagonal :: [String] -> [String]
diagonal [] = []
diagonal xs = [getFirstColumn xs] ++ diagonal (removeFirstColumn xs)

-- 2 of the same numbers in a row means the 3rd should be the opposite number.
double :: String -> String
double s
	| (not((length s) > 2)) = s
	| (head s) == (head (tail s)) && ((head s) /='.') = [(head s)] ++ [(head (tail s))] ++ double ([(opposite (head s))] ++ (tail (tail (tail s))))
	| otherwise = [head s] ++ double (tail s)

--2 of the same numbers and a dot between them, means the opposite number on the place of the dot
between :: String -> String
between s
	| (not((length s) > 2)) = s
	|(head s) == (head (tail(tail s))) && ((head s) /='.') && (head (tail s))=='.' =  [(head s)] ++ [opposite (head s)] ++ between (tail (tail s))
	| otherwise = [(head s)] ++ between (tail s)
	
	
-- when half of the amount of places is filled by a number, the rest should be filled by the opposite number
maximumAmount :: String -> String
maximumAmount s
	| fst(amount s) == div (length s) 2 = fillRest s '1'
	| snd (amount s) == div (length s) 2 = fillRest s '0'
	| otherwise = s
	
fillRest :: String -> Char -> String
fillRest [] c = []
fillRest s c
	| (head s) == '.' = [c] ++ fillRest (tail s) c
	| otherwise = [(head s)] ++ fillRest (tail s) c
	
amount :: String -> (Int, Int)
amount [] = (0,0)
amount s
	| head s == '0' = (1+ fst (amount (tail s)), snd(amount (tail s)))
	| head s == '1' = (fst (amount (tail s)), 1+ snd (amount (tail s)))
	| otherwise = (fst (amount (tail s)), snd(amount (tail s))) 


opposite :: Char -> Char
opposite c
	| c == '0' = '1'
	| c == '1' = '0'
	| otherwise = '.'

main =  print . isCorrectTakuzu .lines =<< getContents
