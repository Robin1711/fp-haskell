{- Insert here your own code. The type of countHappyNumbers must be:

countHappyNumbers :: Integer -> Integer -> Int

-}

wrapper :: [String] -> Int
wrapper (a:b:_) = countHappyNumbers (read a::Integer) (read b::Integer)

main =  print . wrapper . words =<< getLine
