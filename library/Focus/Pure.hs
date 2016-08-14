module Focus.Pure where

import Prelude hiding (adjust, update, alter, insert, delete, lookup)
import Control.Monad


-- |
-- A general modification function for some match.
-- By processing a 'Maybe' value it produces some value to emit and 
-- a 'Instruction' to perform on the match.
-- 
-- The interpretation of this function is up to the context APIs.
type Focus a r = Maybe a -> (r, Instruction a)

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
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:adjust adjust>@.
{-# INLINE adjust #-}
adjust :: (a -> a) -> Focus a ()
adjust f = maybe ((), Keep) (\a -> ((), Set (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:update update>@.
{-# INLINE update #-}
update :: (a -> Maybe a) -> Focus a ()
update f = maybe ((), Keep) (\a -> ((), maybe Remove Set (f a)))

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:alter alter>@.
{-# INLINE alter #-}
alter :: (Maybe a -> Maybe a) -> Focus a ()
alter f = ((),) . maybe Remove Set . f

-- |
-- Reproduces the behaviour of
-- @Data.Map.<http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Lazy.html#v:insert insert>@.
{-# INLINE insert #-}
insert :: a -> Focus a ()
insert a = const ((), Set a)

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

