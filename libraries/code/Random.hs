module Random (
	RandomGen(next, split),
	StdGen, mkStdGen,

	Random( random,   randomR, 
		randoms,  randomRs,
		randomIO, randomRIO ),

	getStdRandom,
	getStdGen, setStdGen, newStdGen
  ) where
	
---------------- The RandomGen class ---------------------------

class RandomGen g where
  next  :: g  -> (Int, g)
  split :: g -> (g, g)		-- May not exist for all RandomGens


---------------- A standard instance of RandomGen ---------------
data StdGen = ...	-- Abstract

instance RandomGen StdGen where ...

-- The show/read instances provide a primitive way to save
-- the state of a random number generator
-- It is expected read (show g) == g

instance Read StdGen where ...
	-- read succeeds on *any* string, not only those
	-- constructed with show.  Hence you can use any
	-- string as way to construct a RandomGen.
	--   - read guarantees to consume a finite portion of
	--     the string
	--   - different strings are likely to result in 
	--     different generators

instance Show StdGen where ...

mkStdGen :: Int -> StdGen
-- Make a StdGen from an Int.  Different Ints should result
-- in different generators.


---------------- The Random class ---------------------------

class Random a where
   randomR :: RandomGen g => (a, a) -> g -> (a, g)
	-- Returns a random value uniformly distributed in [lo,hi]
	-- It is unspecified what happens if lo > hi

   random  :: RandomGen g => g -> (a, g)
	-- Return any value of type a.
	-- For bounded types, the range is normally the whole type
	-- For Fractional types, the range is normally [0..1]
	-- For Integer, the range is (arbitrarily) the range of Int

   randomRs :: RandomGen g => (a, a) -> g -> [a]
   randoms  :: RandomGen g => g -> [a]

   randomIO :: IO a
   randomRIO :: (a,a) -> IO a

	-- Default methods
   randoms g = x : randoms g' 
	     where 
	       (x,g') = random g
   randomRs = ...similar...

   randomIO        = getStdRandom random
   randomRIO range = getStdRandom (randomR range)


instance Random Int     where ...
instance Random Integer where ...
instance Random Float   where ...
instance Random Double  where ...
instance Random Bool    where ...
instance Random Char    where ...


---------------- The global random generator ---------------------------

-- There is a single, implicit, global random number generator
-- of type StdGen, held in some global variable maintained by the IO monad
--
-- It is initialised non-deterministically; to get
-- deterministic behaviour use setStdGen.

setStdGen :: StdGen -> IO ()	-- Set the global generator
getStdGen :: IO StdGen	-- Get the global generator

getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
	-- Use the supplied function to get a value from
	-- the current global random generator, g, and update the
	-- global generator with the new generator returned
getStdRandom f = do
	g <- getStdGen
	let (val, g') = f g
	setStdGen g'
	return val

newStdGen :: IO StdGen
	-- Apply split to the current global random generator
	-- update it with one of the results and return the other
newStdGen = do
	g <- getStdGen
	let (s1,s2) = split g
	setStdGen s1
	return s2


