module Monad (
    MonadPlus(mzero, mplus),
    join, guard, when, unless, ap,
    msum,
    filterM, mapAndUnzipM, zipWithM, zipWithM_, foldM, 
    liftM, liftM2, liftM3, liftM4, liftM5,

    -- ...and what the Prelude exports
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<), 
    ) where

class  Monad m => MonadPlus m  where
    mzero  :: m a
    mplus  :: m a -> m a -> m a

join             :: Monad m => m (m a) -> m a
guard            :: MonadPlus m => Bool -> m ()
when             :: Monad m => Bool -> m () -> m ()
unless           :: Monad m => Bool -> m () -> m ()
ap		 :: Monad m => m (a -> b) -> m a -> m b

mapAndUnzipM     :: Monad m => (a -> m (b,c)) -> [a] -> m ([b], [c])
zipWithM         :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM_        :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
foldM            :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
filterM		 :: Monad m => (a -> m Bool) -> [a] -> m [a]

msum 		 :: MonadPlus m => [m a] -> m a

liftM            :: Monad m => (a -> b) -> (m a -> m b)
liftM2           :: Monad m => (a -> b -> c) -> (m a -> m b -> m c)
liftM3           :: Monad m => (a -> b -> c -> d) ->
                               (m a -> m b -> m c -> m d)
liftM4           :: Monad m => (a -> b -> c -> d -> e) ->
                               (m a -> m b -> m c -> m d -> m e)
liftM5           :: Monad m => (a -> b -> c -> d -> e -> f) ->
                               (m a -> m b -> m c -> m d -> m e -> m f)

