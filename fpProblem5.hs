--polDivision:: [Double] -> [Double] -> ([Double],[Double])
--polDivision dividend divisor
	-- |newFunction dividend divisor [] []
	
	
--newFunction :: [Double] -> [Double] -> [Double] -> [Double] -> ([Double],[Double])
--newFunction dividend divisor quotient remainder
	-- |  = computeQuotient (tail dividend) (tail divisor) (div (head dividend) (head divisor))
	
	
computeQuotient :: [Double] -> [Double] -> [Double] -> ([Double],[Double])
computeQuotient dividend divisor quotient
	| sameLength dividend divisor	= (quotient ++ [divisionNumber], division (tail dividend) (tail divisor) [] divisionNumber)
	| otherwise						= (fst(computeQuotient (division (tail dividend) (tail divisor) [] divisionNumber) divisor (quotient ++ [divisionNumber])), snd(computeQuotient (division (tail dividend) (tail divisor) [] divisionNumber) divisor (quotient ++ [divisionNumber])))
	where divisionNumber = (head dividend)/(head divisor)

division :: [Double] -> [Double] -> [Double] -> Double -> [Double]
division dividend divisor return divisionNumber
	| (tail divisor) == [] 	= return ++ [(head dividend) - divisionNumber * (head divisor)] ++ (tail dividend)
	| otherwise				= division (tail dividend) (tail divisor) (return ++ [(head dividend) - divisionNumber * (head divisor)]) divisionNumber


sameLength:: [Double] -> [Double] -> Bool
sameLength dividend divisor
	| divisor == [] && dividend == []	= True
	| divisor == [] && dividend /= []	= False
	| otherwise 						= sameLength (tail dividend) (tail divisor)
