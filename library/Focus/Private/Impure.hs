module Focus.Private.Impure
where

import Focus.Private.Prelude hiding (adjust, update, alter, insert, delete, lookup, Const)
import qualified Focus.Private.Pure as A


data Focus a m b =
  Const (m (b, A.Decision a)) |
  Lookup (Maybe a -> m (b, A.Decision a))
  deriving (Functor)


{-# INLINE liftPure #-}
liftPure :: Monad m => A.Focus a b -> Focus a m b
liftPure =
  \case
    A.Const output ->
      Const (return output)
    A.Lookup lookupFn ->
      Lookup (return . lookupFn)


-- * Implementations of the common patterns
-------------------------

-- |
-- A monadic version of 'Focus.Pure.adjust'.
{-# INLINE adjust #-}
adjust :: (Monad m) => (a -> m a) -> Focus a m ()
adjust f = 
  Lookup (maybe (return ((), A.Keep)) (liftM (((),) . A.Replace) . f))

-- |
-- A monadic version of 'Focus.Pure.update'.
{-# INLINE update #-}
update :: (Monad m) => (a -> m (Maybe a)) -> Focus a m ()
update f =
  Lookup (maybe (return ((), A.Keep)) (liftM (((),) . maybe A.Remove A.Replace) . f))

-- |
-- A monadic version of 'Focus.Pure.alter'.
{-# INLINE alter #-}
alter :: (Monad m) => (Maybe a -> m (Maybe a)) -> Focus a m ()
alter f =
  Lookup (liftM (((),) . maybe A.Remove A.Replace) . f)

-- |
-- A monadic version of 'Focus.Pure.insert'.
{-# INLINE insert #-}
insert :: (Monad m) => a -> Focus a m ()
insert a =
  Const (return ((), A.Replace a))

-- |
-- A monadic version of 'Focus.Pure.delete'.
{-# INLINE delete #-}
delete :: (Monad m) => Focus a m ()
delete =
  Const (return ((), A.Remove))

-- |
-- A monadic version of 'Focus.Pure.lookup'.
{-# INLINE lookup #-}
lookup :: (Monad m) => Focus a m (Maybe a)
lookup =
  Lookup (fmap return ((,) <$> id <*> const A.Keep))

