module Focus where

import Focus.Prelude hiding (adjust, update, alter, insert, delete, lookup)


{-|
Abstraction over the modification of an element of a datastructure.

It is composable using the standard typeclasses, e.g.:

>lookupAndDelete :: Monad m => Focus a m (Maybe a)
>lookupAndDelete = lookup <* delete
-}
data Focus element m result = Focus (m (result, Maybe element)) (element -> m (result, Maybe element))

deriving instance Functor m => Functor (Focus element m)

instance Monad m => Applicative (Focus element m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Focus element m) where
  return result = Focus (return (result, Nothing)) (\ element -> return (result, Just element))
  (>>=) (Focus aOnNoElement aOnElement) bKleisli = let
    onElement element = do
      (aResult, aOutState) <- aOnElement element
      case bKleisli aResult of
        Focus bOnNoElement bOnElement -> maybe bOnNoElement bOnElement aOutState
    onNoElement = do
      (aResult, aOutState) <- aOnNoElement
      case bKleisli aResult of
        Focus bOnNoElement bOnElement -> maybe bOnNoElement bOnElement aOutState
    in Focus onNoElement onElement


-- * Pure functions
-------------------------

{-|
Lift a pure function on the state of an element, which may as well produce a result.
-}
pureMaybeFn :: Monad m => (Maybe a -> (b, Maybe a)) -> Focus a m b
pureMaybeFn fn = maybeFn (return . fn)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:insert insert>@.
-}
{-# INLINE insert #-}
insert :: Monad m => a -> Focus a m ()
insert a = alter (const (Just a))

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:insertWith insertWith>@
with a better name.
-}
insertOrMerge :: Monad m => (a -> a -> a) -> a -> Focus a m ()
insertOrMerge merge value = alter (Just . maybe value (merge value))

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:alter alter>@.
-}
{-# INLINE alter #-}
alter :: Monad m => (Maybe a -> Maybe a) -> Focus a m ()
alter fn = pureMaybeFn (((),) . fn)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:adjust adjust>@.
-}
{-# INLINE adjust #-}
adjust :: Monad m => (a -> a) -> Focus a m ()
adjust fn = alter (fmap fn)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:update update>@.
-}
{-# INLINE update #-}
update :: Monad m => (a -> Maybe a) -> Focus a m ()
update fn = alter (flip (>>=) fn)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
-}
{-# INLINE[1] lookup #-}
lookup :: Monad m => Focus a m (Maybe a)
lookup = pureMaybeFn (id &&& id)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:findWithDefault findWithDefault>@
with a better name.
-}
{-# INLINE[1] lookupWithDefault #-}
lookupWithDefault :: Monad m => a -> Focus a m a
lookupWithDefault a = pureMaybeFn (maybe a id &&& id)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
-}
{-# INLINE member #-}
member :: Monad m => Focus a m Bool
member = fmap (maybe False (const True)) lookup

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:delete delete>@.
-}
{-# INLINE[1] delete #-}
delete :: Monad m => Focus a m ()
delete = alter (const Nothing)

{-|
Lookup an element and delete it if it exists.

Same as @'lookup' <* 'delete'@.
-}
{-# RULES
  "lookup <* delete" [~1] lookup <* delete = lookupAndDelete
  #-}
lookupAndDelete :: Monad m => Focus a m (Maybe a)
lookupAndDelete = pureMaybeFn (\ state -> (state, Nothing))


-- * Monadic functions
-------------------------

{-|
Lift a monadic function on the state of an element, which may as well produce a result.
-}
maybeFn :: Monad m => (Maybe a -> m (b, Maybe a)) -> Focus a m b
maybeFn fn = Focus (fn Nothing) (fn . Just)
