polDivision:: [Double] -> [Double] -> ([Double],[Double])
polDivision dividends divisors
	|length dividends >= length divisors	= computeQuotient dividends divisors []
	
	
computeQuotient :: [Double] -> [Double] -> [Double] -> ([Double],[Double])
computeQuotient dividends divisors quotients
	| length dividends == length divisors	= (quotients ++ (snd (division (tail dividends) (tail divisors) [] divisionNumber [])), fst (division (tail dividends) (tail divisors) [] divisionNumber []))
	| otherwise								= computeQuotient (fst (division (tail dividends) (tail divisors) [] divisionNumber [])) divisors (quotients ++ snd (division (tail dividends) (tail divisors) [] divisionNumber []))
	where divisionNumber = (head dividends)/(head divisors)

division :: [Double] -> [Double] -> [Double] -> Double -> [Double] ->([Double], [Double])
division dividends divisors remainders divisionNumber quotients
	| divisors == []				= ([], quotients ++ [divisionNumber])
	| remainders == [] && stuff==0 	= division (tail dividends) (tail divisors) (remainders) divisionNumber (quotients ++ [divisionNumber])
	| (tail divisors) == [] 		= (remainders ++ [stuff] ++ (tail dividends), quotients ++ [divisionNumber])
	| otherwise						= (division (tail dividends) (tail divisors) (remainders ++ [stuff]) divisionNumber quotients)
	where stuff = (head dividends) - divisionNumber * (head divisors)

