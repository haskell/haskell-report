module Numeric(fromRat,
               showSigned, showIntAtBase,
               showInt, showOct, showHex,
               readSigned, readInt,
               readDec, readOct, readHex, 
               floatToDigits,
               showEFloat, showFFloat, showGFloat, showFloat, 
               readFloat, lexDigits) where

fromRat        :: (RealFloat a) => Rational -> a

showSigned     :: (Real a) => (a -> ShowS) -> Int -> a -> ShowS
showIntAtBase  :: Integral a => a -> (Int -> Char) -> a -> ShowS
showInt        :: Integral a => a -> ShowS
showOct        :: Integral a => a -> ShowS
showHex        :: Integral a => a -> ShowS

readSigned     :: (Real a) => ReadS a -> ReadS a
readInt        :: (Integral a) =>
                    a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readDec        :: (Integral a) => ReadS a
readOct        :: (Integral a) => ReadS a
readHex        :: (Integral a) => ReadS a

showEFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showGFloat     :: (RealFloat a) => Maybe Int -> a -> ShowS
showFloat      :: (RealFloat a) => a -> ShowS

floatToDigits  :: (RealFloat a) => Integer -> a -> ([Int], Int)

readFloat      :: (RealFloat a) => ReadS a
lexDigits      :: ReadS String 
