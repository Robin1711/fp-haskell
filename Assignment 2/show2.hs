import Data.Char
import Data.List

type Name = String
type Domain = [Integer]
type Valuation = [(Name,Integer)]

data Expr = Val Integer
	| Var Name
	| Expr :+: Expr
	| Expr :-: Expr
	| Expr :*: Expr
	| Expr :/: Expr
	| Expr :%: Expr

------- Exercise 1:	-----------------------------

par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Expr where
	show (Val i) = [intToDigit (fromInteger i)]
	show (Var x) = x
	show (p :+: q) = par(show p ++ "+" ++ show q)
	show (p :-: q) = par(show p ++ "-" ++ show q)
	show (p :*: q) = par(show p ++ "*" ++ show q)
	show (p :/: q) = par(show p ++ "/" ++ show q)
	show (p :%: q) = par(show p ++ "%" ++ show q)

------- Exercise 2:	-----------------------------

vars :: Expr -> [String]
vars expr = sort (rmduplicates (variables expr))

variables :: Expr -> [String]
variables (Val i) = []
variables (Var x) = [x]
variables (p :+: q) = (variables p) ++ (variables q)
variables (p :-: q) = (variables p) ++ (variables q)
variables (p :*: q) = (variables p) ++ (variables q)
variables (p :/: q) = (variables p) ++ (variables q)
variables (p :%: q) = (variables p) ++ (variables q)

rmduplicates :: Eq a => [a] -> [a]
rmduplicates [] = []
rmduplicates (x:xs)
	| elem x xs = rmduplicates xs
	| otherwise = x : rmduplicates xs

------- Exercise 3:	-----------------------------

evalExpr :: Expr -> Valuation -> Integer
evalExpr e vals
	| verify (vars e) (map fst vals) = calc e vals
	| otherwise = error "not all variables are values assigned"

verify :: [String] -> [Name] -> Bool
verify [] _ = True
verify (x:xs) ys
	| elem x ys = verify xs ys
	| otherwise = False

calc :: Expr -> Valuation -> Integer
calc (Val i) _ = i
calc (Var x) v = getVal x v 
calc (p :+: q) v = (calc p v) + (calc q v)
calc (p :-: q) v = (calc p v) - (calc q v)
calc (p :*: q) v = (calc p v) * (calc q v)
calc (p :/: q) v = div (calc p v) (calc q v)
calc (p :%: q) v = mod (calc p v) (calc q v)

getVal :: String -> Valuation -> Integer
getVal _ [] = error "variable is not in valuation --> verify is not correct"
getVal s (x:xs)
	| s == fst x = snd x
	| otherwise = getVal s xs

----------	valuations	-------------------------

valuations :: [(Name,Domain)] -> [Valuation]
valuations ((n,d):xs)
	| d == [] = []
	| otherwise = combine [(n, head d)] xs ++ valuations ([(n, tail d)] ++ xs)

combine :: Valuation -> [(Name,Domain)] -> [Valuation]
combine v [] = [v]
combine v ((n,d):xs)
	| xs == [] && d == [] = []
	| otherwise = [v ++ x | x <- valuations ((n,d):xs)]

----------	pytriples	-------------------------

pytriples :: Integer -> [Valuation]
pytriples n = validateTriples (computeTriples n)
	where
		validateTriples :: [Valuation] -> [Valuation]
		validateTriples xs = [x | x <- xs, isCorrect x, isPytriple x]

computeTriples :: Integer -> [Valuation]
computeTriples n = valuations [("a",[1..n]), ("b",[1..n]), ("c",[1..n])]

isCorrect :: Valuation -> Bool
isCorrect val = snd (head val) <= snd (head (tail val))

isPytriple :: Valuation -> Bool
isPytriple val = evalExpr e1 val == evalExpr e2 val
	where { e1 = (Var "a" :*: Var "a") :+: (Var "b" :*: Var "b");
			e2 = (Var "c" :*: Var "c")}

------- Exercise 5:	-----------------------------
--toExpr :: String -> Expr
--toExpr s = parse (tokenize s)

--    toExpr "2*a+b" ----> ((2*a)+b)  
--(2 * a) + b
--takWhile notOperator 

--takeWhile f s == 2
--head (dropWhile notOperator s) == o
--head (tail (dropWhile f s)) == a
--tail (tail (dropWhile f s)) == "+", "b"

{-
parse :: [String] -> Expr
parse s
	| tail s == [] && isAllNumber (head s) = Val (toNumber (head s))
	| tail s == [] && all isVariable (head s) = Var (head s)
	| o == "+" = parse (takeWhile f s) :+: parse (tail (dropWhile f s))
	| o == "-" = parse (takeWhile f s) :-: parse (tail (dropWhile f s))
	| o == "*" = parse (takeWhile f s) :*: parse (tail (dropWhile f s))
	| o == "/" = parse (takeWhile f s) :/: parse (tail (dropWhile f s))
	| o == "%" = parse (takeWhile f s) :%: parse (tail (dropWhile f s))
	| otherwise = error("We came here")
	where { o = head (dropWhile notOperator s);
			f = notOperator}

isAllNumber :: String -> Bool
isAllNumber s = all isNumber s

notOperator :: String -> Bool
notOperator (x:xs)
	| x == '+' = False
	| x == '-' = False
	| x == '*' = False
	| x == '/' = False
	| x == '%' = False
	| otherwise = True

toNumber :: String -> Integer
toNumber (x:[]) = toInteger (digitToInt x)
toNumber (x:xs) = ((toInteger (digitToInt x))*10^(length xs)) + toNumber xs

tokenize :: String -> [String]
tokenize "" = []
tokenize s 
	| head s == ' ' = tokenize (tail s)
	| isNumber (head s) = [takeWhile isNumber s] ++ tokenize (dropWhile isNumber s)
	| isVariable (head s) = [takeWhile isVariable s] ++ tokenize (dropWhile isVariable s)
	| otherwise = [[head s]] ++ tokenize (tail s)

--isNumber :: Char -> Bool
--isNumber c = (fromEnum c < 58 && 47<fromEnum c)

isVariable :: Char -> Bool
isVariable c = (fromEnum c < 91 && 64<fromEnum c) || (fromEnum c < 123 && 96<fromEnum c)

--(Var "x") :+: (Val 2 :*: Val 3)
-}

---------------------------------------
toExpr :: String -> Expr
toExpr s = fst (parser s)

parser :: String -> (Expr,[String])
parser str = parseE (Var "a") (tokenize str)

{-
parser :: String -> (Expr,[String])
parser str = parseE e s
	where (e,s) = parseF (tokenize (tail str))
-}

parseE:: Expr -> [String] -> (Expr,[String])
parseE accepted tokens = parseE' acc rest
	where (acc,rest) = parseT accepted tokens

parseE' :: Expr -> [String] -> (Expr,[String])
parseE' accepted ("+":tokens) = parseE' acc rest
	where (acc,rest) = parseT (accepted :+: expr) leftover
		where (expr,leftover) = parseF tokens
parseE' accepted ("-":tokens) = parseE' acc rest
	where (acc,rest) = parseT (accepted :-: expr) leftover
		where (expr,leftover) = parseF tokens
parseE' accepted tokens = (accepted, tokens)
	
parseT:: Expr -> [String] -> (Expr,[String])
parseT accepted tokens = parseT' acc rest
	where (acc,rest) = parseF tokens

parseT' :: Expr -> [String] -> (Expr,[String])
parseT' accepted ("*":tokens) = parseT' acc rest
	where (acc,rest) = parseT (accepted :*: expr) leftover
		where (expr,leftover) = parseF tokens
parseT' accepted ("/":tokens) = parseT' acc rest
	where (acc,rest) = parseT (accepted :/: expr) leftover
		where (expr,leftover) = parseF tokens
parseT' accepted ("%":tokens) = parseT' acc rest
	where (acc,rest) = parseT (accepted :%: expr) leftover
		where (expr,leftover) = parseF tokens
parseT' accepted tokens = (accepted, tokens)

parseF :: [String] -> (Expr,[String])
parseF [] = error "Parse error ... abort"
parseF (tok:tokens)
	| isAlpha (head tok) = (Var tok, tokens)
	| isDigit (head tok) = (Val (toNumber tok), tokens)
	| otherwise = (fst(parser tok), tokens)

toNumber :: String -> Integer
toNumber (x:[]) = toInteger (digitToInt x)
toNumber (x:xs) = ((toInteger (digitToInt x))*10^(length xs)) + toNumber xs

tokenize :: String -> [String]
tokenize "" = []
tokenize s 
	| head s == ' ' = tokenize (tail s)
	| isNumber (head s) = [takeWhile isNumber s] ++ tokenize (dropWhile isNumber s)
	| isVariable (head s) = [takeWhile isVariable s] ++ tokenize (dropWhile isVariable s)
	| otherwise = [[head s]] ++ tokenize (tail s)
	
isVariable :: Char -> Bool
isVariable c = (fromEnum c < 91 && 64<fromEnum c) || (fromEnum c < 123 && 96<fromEnum c)