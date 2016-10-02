polDivision:: [Double] -> [Double] -> ([Double],[Double])
polDivision dividends divisors
	| equalsZero divisors = error "Dividends or divisors start with 0.0"
	| (head divisors) == 0 = error "Head divisors starts with 0.0"
	| (head dividends) == 0 = error "Head dividends starts with 0.0"
	| length dividends < length divisors = error "Length divisors greater than length dividends"
	| otherwise = (computeQuotient dividends divisors, computeRemainder dividends divisors) -- if it is a correct polynomial division start calculation

computeRemainder :: [Double] -> [Double] -> [Double]
computeRemainder dividends divisors
	| length dividends < length divisors 	= dividends
	| length dividends == length divisors 	= oneDivision dividends divisors divisionNumber
	| otherwise								= computeRemainder (oneDivision dividends divisors divisionNumber) divisors
	where divisionNumber = (head dividends) / (head divisors)
	
computeQuotient :: [Double] -> [Double] -> [Double]
computeQuotient dividends divisors
	| length dividends < length divisors 	= []
	| length dividends == length divisors 	= [divisionNumber]
	| otherwise								= [divisionNumber] ++ computeQuotient (oneDivision dividends divisors divisionNumber) divisors
	where divisionNumber = (head dividends) / (head divisors)
	
oneDivision :: [Double] -> [Double] -> Double -> [Double]
oneDivision dividends divisors n = removeZeroes (minus dividends (convertDivisor (matchLength dividends divisors) n))

minus :: [Double] -> [Double] -> [Double]
minus [] xs = []
minus (d:dividends) (e:divisors) = [d-e] ++ minus dividends divisors 
	
removeZeroes :: [Double] -> [Double] --removes starting zeroes
removeZeroes [] = []
removeZeroes (x:xs)
	| x==0	= removeZeroes xs
	| otherwise = [x]++xs

matchLength:: [Double] -> [Double] -> [Double] --this function returns divisor of same length as dividend
matchLength [] xs = []
matchLength (x:xs) [] = [0] ++ matchLength xs []
matchLength (d:dividends) (e: divisors) = [e] ++ matchLength dividends divisors
	
convertDivisor :: [Double] -> Double -> [Double] -- this function returns divisor times divisionNumber
convertDivisor [] n = []
convertDivisor (x:xs) n = [x*n] ++ convertDivisor xs n

equalsZero :: [Double] -> Bool
equalsZero [] = True
equalsZero (x:xs)
	| x == 0 = equalsZero xs
	| otherwise = False


wrapper :: String -> ([Double],[Double])
wrapper line = polDivision (makeList num) (makeList denom)
  where
    num = takeWhile (/= '/') line
    denom = tail (dropWhile (/= '/') line)
    makeList str = map (\s -> read s::Double) (words str)

main =  print . wrapper =<< getLine
--main = do  -- print . wrapper =<< getLine
--    line <- getLine
--    if line `notElem` []++getLine then 
--        error ("unseen input: " ++ show line)
--    else
--        print $ wrapper line
--        