module  Complex ( 
    Complex((:+)), realPart, imagPart, conjugate, 
    mkPolar, cis, polar, magnitude, phase ) where

infix  6  :+

data  (RealFloat a)     => Complex a = !a :+ !a

realPart, imagPart      :: (RealFloat a) => Complex a -> a
conjugate	        :: (RealFloat a) => Complex a -> Complex a
mkPolar		        :: (RealFloat a) => a -> a -> Complex a
cis		        :: (RealFloat a) => a -> Complex a
polar		        :: (RealFloat a) => Complex a -> (a,a)
magnitude, phase        :: (RealFloat a) => Complex a -> a

instance  (RealFloat a) => Eq         (Complex a)  where ...
instance  (RealFloat a) => Read       (Complex a)  where ...
instance  (RealFloat a) => Show       (Complex a)  where ...
instance  (RealFloat a) => Num        (Complex a)  where ...
instance  (RealFloat a) => Fractional (Complex a)  where ...
instance  (RealFloat a) => Floating   (Complex a)  where ...
