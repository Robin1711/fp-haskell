
{-
Hoe ik het in gedachten had:
Bij isCorrectTakuzu
Een or statement met vul 0 en 1 in op de eerste positie.

In fill adden we het eerste karakter en checken we of
de takuzu nog klopt en returenen we False of True

Add voegt de 0 of de 1 in op de eerste vrije positie.
tryAdd probeert het karakter in de string toe te voegen.
In add checken we of dat gelukt is door de strings te vergelijken.

Ik probeerde een tuple terug te geven, maar dat kan niet omdat je
dan ingewikkeld de niewue string op moet bouwen.

Check checkt of de huidige Takuzu nog klopt.


Dit is nog niet compleet, want je moet ook nog iets doen met
even en oneven rijen. En checken op een even aantal enen en
nullen. Zowel verticaal als horizontaal. En dat gebeurt nu nog niet.

checkString heb ik uit Problem 2 gehaald en een beetje aangepast
zodat ie de puntjes niet meerekent.
-}





--isCorrectTakuzu :: [String] -> Bool
--isCorrectTakuzu strings = (fill strings '0') || (fill strings '1')

--fill :: [String] -> Char -> [Bool]
--fill strings c
--	| check new = 
--	| otherwise = 
--where new = add strings c

add :: [String] -> Char -> [String]
add (x:xs) c
	| new == x = [x] ++ (add xs c)
	| otherwise = [new] ++ xs
	where new = tryAdd x c

tryAdd :: String -> Char -> String
tryAdd [] _ = []
tryAdd (x:xs) c
	| x == '.' = [c] ++ xs
	| otherwise = [x] ++ (tryAdd xs c)

checkTakuzu :: [String] -> Bool
checkTakuzu [] = True
checkTakuzu (s:strings)
	| checkString s ' ' 0 = checkTakuzu strings
	| otherwise = False

checkString :: String -> Char -> Int -> Bool
checkString s l amount
	| (head s) == l && amount == 2 = False
	| tail s == "" = True
	| head s == '.' = checkString (tail s) l 0
	| head s == l = checkString (tail s) l (amount+1)
	| otherwise = checkString (tail s) (head s) 1

toInt :: Char -> Int
toInt c = (fromEnum c) - 48

toChar :: Int -> Char
toChar n = toEnum (n + 48)
