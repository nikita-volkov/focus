module Focus where

import Focus.Prelude hiding (adjust, update, alter, insert, delete, lookup)


-- |
-- A general modification function for some match.
-- By processing a 'Maybe' value it produces some value to emit and 
-- a 'Decision' to perform on the match.
-- 
-- The interpretation of this function is up to the context APIs.
type Strategy a r = Maybe a -> (r, Decision a)

-- |
-- A monadic version of 'Strategy'.
type StrategyM m a r = Maybe a -> m (r, Decision a)

-- |
-- What to do with the focused value.
-- 
-- The interpretation of the commands is up to the context APIs.
data Decision a =
  Keep |
  Remove |
  Replace a
  deriving (Functor)


-- * Constructors for common pure patterns
-------------------------

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:adjust adjust>@.
adjust :: (a -> a) -> Strategy a ()
adjust f = maybe ((), Keep) (\a -> ((), Replace (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:update update>@.
update :: (a -> Maybe a) -> Strategy a ()
update f = maybe ((), Keep) (\a -> ((), maybe Remove Replace (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:alter alter>@.
alter :: (Maybe a -> Maybe a) -> Strategy a ()
alter f = ((),) . maybe Remove Replace . f

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:insert insert>@.
insert :: a -> Strategy a ()
insert a = const ((), Replace a)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:delete delete>@.
delete :: Strategy a ()
delete = const ((), Remove)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
lookup :: Strategy a (Maybe a)
lookup r = (r, Keep)


-- * Constructors for monadic patterns
-------------------------

-- |
-- A monadic version of 'adjust'.
adjustM :: (Monad m) => (a -> m a) -> StrategyM m a ()
adjustM f = maybe (return ((), Keep)) (liftM (((),) . Replace) . f)

-- |
-- A monadic version of 'update'.
updateM :: (Monad m) => (a -> m (Maybe a)) -> StrategyM m a ()
updateM f = maybe (return ((), Keep)) (liftM (((),) . maybe Remove Replace) . f)

-- |
-- A monadic version of 'alter'.
alterM :: (Monad m) => (Maybe a -> m (Maybe a)) -> StrategyM m a ()
alterM f = liftM (((),) . maybe Remove Replace) . f

-- |
-- A monadic version of 'insert'.
insertM :: (Monad m) => a -> StrategyM m a ()
insertM = fmap return . insert

-- |
-- A monadic version of 'delete'.
deleteM :: (Monad m) => StrategyM m a ()
deleteM = fmap return delete

-- |
-- A monadic version of 'lookup'.
lookupM :: (Monad m) => StrategyM m a (Maybe a)
lookupM = fmap return lookup


