{- Insert here your own code. The type of your functions must be:

cipherEncode :: Int -> String -> String
cipherDecode :: Int -> String -> String

-}

wrapper :: String -> String
wrapper line
  | cmd == "ENCODE"  = cipherEncode key txt
  | cmd == "DECODE"  = cipherDecode key txt
  where
    str  = dropWhile (not.isAlpha) line
    cmd  = takeWhile isAlpha str
    tail = dropWhile (not.isDigit) str
    key = read (takeWhile isDigit tail)::Int
    txt = dropWhile (not.isAlpha) (dropWhile isDigit tail)

main =  print . wrapper =<< getLine
