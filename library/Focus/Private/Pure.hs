module Focus.Private.Pure where

import Focus.Private.Prelude hiding (adjust, update, alter, insert, delete, lookup, Const)


data Focus a b =
  Const (b, Decision a) |
  Lookup (Maybe a -> (b, Decision a))

-- |
-- What to do with the focused value.
-- 
-- The interpretation of the commands is up to the context APIs.
data Decision a =
  Keep |
  Remove |
  Replace a
  deriving (Functor)


-- * Implementations of the common patterns
-------------------------

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:adjust adjust>@.
{-# INLINE adjust #-}
adjust :: (a -> a) -> Focus a ()
adjust f =
  Lookup (maybe ((), Keep) (\a -> ((), Replace (f a))))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:update update>@.
{-# INLINE update #-}
update :: (a -> Maybe a) -> Focus a ()
update f =
  Lookup (maybe ((), Keep) (\a -> ((), maybe Remove Replace (f a))))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:alter alter>@.
{-# INLINE alter #-}
alter :: (Maybe a -> Maybe a) -> Focus a ()
alter f =
  Lookup (((),) . maybe Remove Replace . f)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:insert insert>@.
{-# INLINE insert #-}
insert :: a -> Focus a ()
insert a =
  Const ((), Replace a)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:delete delete>@.
{-# INLINE delete #-}
delete :: Focus a ()
delete =
  Const ((), Remove)

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
{-# INLINE lookup #-}
lookup :: Focus a (Maybe a)
lookup =
  Lookup (\a -> (a, Keep))

