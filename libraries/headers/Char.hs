module Char ( 
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower, 
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum, 
    digitToInt, intToDigit,
    toUpper, toLower,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,

	-- ...and what the Prelude exports
    Char, String
    ) where

isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower, 
 isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum :: Char -> Bool

toUpper, toLower        :: Char -> Char

digitToInt :: Char -> Int
intToDigit :: Int -> Char

ord        :: Char -> Int
chr        :: Int  -> Char

lexLitChar  :: ReadS String
readLitChar :: ReadS Char
showLitChar :: Char -> ShowS
