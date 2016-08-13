module Focus where

import Prelude hiding (adjust, update, alter, insert, delete, lookup)
import Control.Monad


-- |
-- A general modification function for some match.
-- By processing a 'Maybe' value it produces some value to emit and 
-- a 'Decision' to perform on the match.
-- 
-- The interpretation of this function is up to the context APIs.
type Focus a r = Maybe a -> (r, Decision a)

-- |
-- A monadic version of 'Focus'.
type FocusM a m r = Maybe a -> m (r, Decision a)

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
{-# INLINE adjust #-}
adjust :: (a -> a) -> Focus a ()
adjust f = maybe ((), Keep) (\a -> ((), Replace (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:update update>@.
{-# INLINE update #-}
update :: (a -> Maybe a) -> Focus a ()
update f = maybe ((), Keep) (\a -> ((), maybe Remove Replace (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:alter alter>@.
{-# INLINE alter #-}
alter :: (Maybe a -> Maybe a) -> Focus a ()
alter f = ((),) . maybe Remove Replace . f

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:insert insert>@.
{-# INLINE insert #-}
insert :: a -> Focus a ()
insert a = const ((), Replace a)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:delete delete>@.
{-# INLINE delete #-}
delete :: Focus a ()
delete = const ((), Remove)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
{-# INLINE lookup #-}
lookup :: Focus a (Maybe a)
lookup r = (r, Keep)


-- * Constructors for monadic patterns
-------------------------

-- |
-- A monadic version of 'adjust'.
{-# INLINE adjustM #-}
adjustM :: (Monad m) => (a -> m a) -> FocusM a m ()
adjustM f = maybe (return ((), Keep)) (liftM (((),) . Replace) . f)

-- |
-- A monadic version of 'update'.
{-# INLINE updateM #-}
updateM :: (Monad m) => (a -> m (Maybe a)) -> FocusM a m ()
updateM f = maybe (return ((), Keep)) (liftM (((),) . maybe Remove Replace) . f)

-- |
-- A monadic version of 'alter'.
{-# INLINE alterM #-}
alterM :: (Monad m) => (Maybe a -> m (Maybe a)) -> FocusM a m ()
alterM f = liftM (((),) . maybe Remove Replace) . f

-- |
-- A monadic version of 'insert'.
{-# INLINE insertM #-}
insertM :: (Monad m) => a -> FocusM a m ()
insertM = fmap return . insert

-- |
-- A monadic version of 'delete'.
{-# INLINE deleteM #-}
deleteM :: (Monad m) => FocusM a m ()
deleteM = fmap return delete

-- |
-- A monadic version of 'lookup'.
{-# INLINE lookupM #-}
lookupM :: (Monad m) => FocusM a m (Maybe a)
lookupM = fmap return lookup


