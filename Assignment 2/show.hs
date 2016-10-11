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
valuations (x:xs)
	| (snd x) == [] = []
	| otherwise = combine [(fst x, head (snd x))] xs ++ valuations ([(fst x, tail (snd x))] ++ xs)

combine :: Valuation -> [(Name,Domain)] -> [Valuation]
combine v [] = [v]
combine v (x:xs)
	| xs == [] && (snd x) == [] = []
	|otherwise = [v ++ [(fst x, head (snd x))]] ++ combine v ([(fst x, tail (snd x))] ++ xs)
	
----------	pytriples	-------------------------
{-
pytriples :: Integer -> [Valuation]
pytriples n = validateTriples (computeTriples n)
	where
		validateTriples :: [Valuation] -> [Valuation]
		validateTriples (pt:xs)= pt : validateTriples [x | x <- xs, isPytriple x]
-}

computeTriples :: Integer -> [Valuation]
computeTriples n = filter isCorrect (valuations [("a",[1..n]), ("b",[1..n]), ("c",[1..n])])

isCorrect :: Valuation -> Bool
isCorrect val = snd (head val) < snd (head (tail val))

{-
isPytriple :: Valuation -> Bool
isPytriple (v:vs)

pytriples :: Integer -> [Valuation]
pytriples n
	| func ((Var "a" :*: Var "a") :+: (Var "b" :*: Var "b")) (Var "c" :*: Var "c") (correct (valuations [("a",[1..n]),("b",[1..n])])) (valuations[("c",[1..n])])

func :: Expr -> Expr -> [Valuation] -> [Valuations]
func e1 e2 xs
  (
-}

correct :: [Valuation] -> [Valuation]
correct (x:xs)
	| snd (head x) <= snd (head(tail x)) = [x] ++ correct xs
	| otherwise = correct xs

------- Exercise 5:	-----------------------------
toExpr :: String -> Expr
toExpr s = parse (tokenize s)

{-

--parseE :: [String] -> Expr
--parseE (x:xs)
--	| o == "+" = parseT x :+: parseEp
--	| o == "-" = parseT x :-: parseEp
--	| o == "*" = parseT x :*: parseEp
--	| o == "/" = parseT x :/: parseEp
--	| o == "%" = parseT x :%: parseEp
--	where o = (head xs)

parser

parseE :: [String] -> Expr
parseE (x:xs) = parseT parseEp

parseEp :: [String] -> Expr
parseEp (x:xs)
	| x == "+" = :+: parseT parseEp
	| x == "-" = :-: parseT parseEp
	| otherwise = 

parseT :: [String] -> Expr
parseT (x:xs) = parseF parseTp

parseTp :: [String] -> Expr
parseTp (x:xs)
	| x == "*" = :*: parseT parseEp
	| x == "/" = :/: parseT parseEp
	| x == "%" = :%: parseT parseEp
	| otherwise = 

parseF :: [String] -> Expr
parseF (x:xs)
	| xs == [] && (all isNumber x) = Var (head s) -- <variable>
	| xs == [] && (all isVariable x) = Val (toNumber (head s)) -- <integer> 
	| otherwise = parseE (x:xs)

{-

E 	->	T E’
E’ 	-> 	+ T E’ | - T E' | <empty string>
T 	->	F T’
T’ 	-> 	* F T’ | / F T’ | % F T’ | <empty string>
F 	-> 	(E) | <integer> | <variable>

-}

--    toExpr "2*a+b" ----> ((2*a)+b)  
--(2 * a) + b
--takWhile notOperator 

--takeWhile f s == 2
--head (dropWhile notOperator s) == o
--head (tail (dropWhile f s)) == a
--tail (tail (dropWhile f s)) == "+", "b"


-}

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
