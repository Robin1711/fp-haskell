rpnEval:: String -> Int
rpnEval s = calc s []

calc :: String -> [Int] -> Int
calc [] stack = head stack
calc s stack
	| head s == ' '				= calc (tail s) stack
	| getCharacter (head s) < 0 = calc (tail s) ((useOp (head bottom) (head stack) (getCharacter (head s))) : (tail bottom))
	| otherwise					= calc (snd(getNumber s 0)) ((fst(getNumber s 0)) : stack)
	where bottom = tail stack

getNumber :: String -> Int -> (Int, String)
getNumber [] i = (i,[])
getNumber (x:xs) i
	| getCharacter x >= 0	= getNumber xs (i*10 + getCharacter x)
	| otherwise				= (i, s)
	where s = [x]++xs
	
getCharacter :: Char -> Int
getCharacter x
	| charIsNum x 		= charToNum x
	| charIsOperator x	= charToOperator x
	| otherwise			= -5

useOp :: Int -> Int -> Int -> Int
useOp i j o
	| o == -1 			= i + j
	| o == -2			= i - j
	| o == -3 			= i * j
	| o == -4 && j /= 0 = div i j
	| otherwise = error "Division by zero" -- if the divisor j is 0 we return ????

charIsNum:: Char -> Bool
charIsNum ch
	|(ch >='0') && (ch <= '9')	= True
	| otherwise					= False
	
charIsOperator:: Char -> Bool
charIsOperator ch
	| ch == '+'	= True
	| ch == '-'	= True
	| ch == '*'	= True
	| ch == '/'	= True
	| otherwise	= False

charToNum:: Char -> Int
charToNum ch = fromEnum ch - 48
	
charToOperator:: Char -> Int
charToOperator ch
	| ch == '+'	= -1
	| ch == '-'	= -2
	| ch == '*'	= -3
	| ch == '/'	= -4

main =  print . rpnEval =<< getLine
