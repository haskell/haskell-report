module Ix ( Ix(range, index, inRange), rangeSize ) where

class  Ix a  where
    range     :: (a,a) -> [a]
    index     :: (a,a) -> a -> Int
    inRange   :: (a,a) -> a -> Bool
    rangeSize :: (a,a) -> Int

    rangeSize b@(l,h) | null (range b) = 0
                      | otherwise      = index b h + 1 
	-- NB: replacing "null (range b)" by  "l > h" fails if
	-- the bounds are tuples.  For example,
	-- 	(2,1) > (1,2), 
	-- but 
	--	range ((2,1),(1,2)) = []


instance  Ix Char  where
    range (m,n)		= [m..n]
    index b@(c,c') ci
        | inRange b ci  =  fromEnum ci - fromEnum c
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (c,c') i    =  c <= i && i <= c'

instance  Ix Int  where
    range (m,n)		= [m..n]
    index b@(m,n) i
        | inRange b i   =  i - m
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (m,n) i     =  m <= i && i <= n

instance  Ix Integer  where
    range (m,n)		= [m..n]
    index b@(m,n) i
        | inRange b i   =  fromInteger (i - m)
        | otherwise     =  error "Ix.index: Index out of range."
    inRange (m,n) i     =  m <= i && i <= n

instance (Ix a,Ix b) => Ix (a, b) -- as derived, for all tuples
instance Ix Bool                  -- as derived
instance Ix Ordering              -- as derived
instance Ix ()                    -- as derived
