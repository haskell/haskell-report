intersect           :: Eq a => [a] -> [a] -> [a]
intersectBy         :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersperse         :: a -> [a] -> [a]
transpose           :: [[a]] -> [[a]]
partition           :: (a -> Bool) -> [a] -> ([a],[a])
group               :: Eq a => [a] -> [[a]]
groupBy             :: (a -> a -> Bool) -> [a] -> [[a]]
inits               :: [a] -> [[a]] 
tails               :: [a] -> [[a]] 
isPrefixOf          :: Eq a => [a] -> [a] -> Bool
isSuffixOf          :: Eq a => [a] -> [a] -> Bool
mapAccumL           :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR           :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
unfoldr		    :: (b -> Maybe (a,b)) -> b -> [a]
sort                :: Ord a => [a] -> [a]
sortBy              :: (a -> a -> Ordering) -> [a] -> [a]
insert		    :: Ord a => a -> [a] -> [a]
insertBy            :: (a -> a -> Ordering) -> a -> [a] -> [a]
maximumBy           :: (a -> a -> Ordering) -> [a] -> a
minimumBy           :: (a -> a -> Ordering) -> [a] -> a
genericLength       :: Integral a => [b] -> a
genericTake	    :: Integral a => a -> [b] -> [b]
genericDrop	    :: Integral a => a -> [b] -> [b]
genericSplitAt	    :: Integral a => a -> [b] -> ([b],[b])
genericIndex	    :: Integral a => [b] -> a -> b
genericReplicate    :: Integral a => a -> b -> [b]

zip4                :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip5                :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip6                :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] 
                          -> [(a,b,c,d,e,f)]
zip7                :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
                          -> [(a,b,c,d,e,f,g)]
zipWith4            :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith5            :: (a->b->c->d->e->f) -> 
                       [a]->[b]->[c]->[d]->[e]->[f]
zipWith6            :: (a->b->c->d->e->f->g) ->
                       [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith7            :: (a->b->c->d->e->f->g->h) ->
                       [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
unzip4              :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip5              :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip6              :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip7              :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
