module Focus.Impure
where

import Prelude hiding (adjust, update, alter, insert, delete, lookup)
import Control.Monad


-- |
-- A monadic version of 'Focus.Pure.Focus.Pure.Focus'.
type Focus a m r = Maybe a -> m (r, Instruction a)

-- |
-- What to do with the focused value.
-- 
-- The interpretation of the commands is up to the context APIs.
data Instruction a =
  Keep |
  Remove |
  Set a
  deriving (Functor)


-- * Implementations of the common patterns
-------------------------

-- |
-- A monadic version of 'Focus.Pure.adjust'.
{-# INLINE adjust #-}
adjust :: (Monad m) => (a -> m a) -> Focus a m ()
adjust f = maybe (return ((), Keep)) (liftM (((),) . Set) . f)

-- |
-- A monadic version of 'Focus.Pure.update'.
{-# INLINE update #-}
update :: (Monad m) => (a -> m (Maybe a)) -> Focus a m ()
update f = maybe (return ((), Keep)) (liftM (((),) . maybe Remove Set) . f)

-- |
-- A monadic version of 'Focus.Pure.alter'.
{-# INLINE alter #-}
alter :: (Monad m) => (Maybe a -> m (Maybe a)) -> Focus a m ()
alter f = liftM (((),) . maybe Remove Set) . f

-- |
-- A monadic version of 'Focus.Pure.insert'.
{-# INLINE insert #-}
insert :: (Monad m) => a -> Focus a m ()
insert a = fmap return (const ((), Set a))

-- |
-- A monadic version of 'Focus.Pure.delete'.
{-# INLINE delete #-}
delete :: (Monad m) => Focus a m ()
delete = fmap return (const ((), Remove))

-- |
-- A monadic version of 'Focus.Pure.lookup'.
{-# INLINE lookup #-}
lookup :: (Monad m) => Focus a m (Maybe a)
lookup = fmap return ((,) <$> id <*> const Keep)

