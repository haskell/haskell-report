module Random (
	RandomGen(next, split),
	StdGen, mkStdGen,
	Random( random,   randomR, 
		randoms,  randomRs,
		randomIO, randomRIO ),
	getStdRandom, getStdGen, setStdGen, newStdGen
  ) where
	
---------------- The RandomGen class ------------------------

class RandomGen g where
  next  :: g  -> (Int, g)
  split :: g -> (g, g)

---------------- A standard instance of RandomGen -----------
data StdGen = ...	-- Abstract

instance RandomGen StdGen where ...
instance Read 	   StdGen where ...
instance Show 	   StdGen where ...

mkStdGen :: Int -> StdGen

---------------- The Random class ---------------------------
class Random a where
   randomR :: RandomGen g => (a, a) -> g -> (a, g)
   random  :: RandomGen g => g -> (a, g)

   randomRs :: RandomGen g => (a, a) -> g -> [a]
   randoms  :: RandomGen g => g -> [a]

   randomRIO :: (a,a) -> IO a
   randomIO  :: IO a

instance Random Int     where ...
instance Random Integer where ...
instance Random Float   where ...
instance Random Double  where ...
instance Random Bool    where ...
instance Random Char    where ...

---------------- The global random generator ----------------
newStdGen    :: IO StdGen
setStdGen    :: StdGen -> IO ()
getStdGen    :: IO StdGen	
getStdRandom :: (StdGen -> (a, StdGen)) -> IO a


