{- Insert here your own code. The type of takuzuStrings must be:

takuzuStrings :: Integer-> [String]

-}

wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
