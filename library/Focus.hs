module Focus where

import Focus.Prelude hiding (adjust, update, alter, insert, delete, lookup)


{-|
Abstraction over the modification of an element of a datastructure.

It is composable using the standard typeclasses, e.g.:

>lookupAndDelete :: Monad m => Focus a m (Maybe a)
>lookupAndDelete = lookup <* delete
-}
newtype Focus element m result = Focus (Maybe element -> m (result, Maybe element))

deriving instance Functor m => Functor (Focus element m)

instance Monad m => Applicative (Focus element m) where
  pure result = Focus (\ state -> pure (result, state))
  (<*>) = ap

instance Monad m => Monad (Focus element m) where
  return = pure
  (>>=) (Focus focusA) bKleisli =
    Focus $ \ state -> do
      (aResult, aOutState) <- focusA state
      case bKleisli aResult of
        Focus focusB -> focusB aOutState

adjust :: Monad m => (a -> a) -> Focus a m ()
adjust fn = Focus (pure . ((),) . fmap fn)

update :: Monad m => (a -> Maybe a) -> Focus a m ()
update fn = Focus (pure . ((),) . flip (>>=) fn)

{-# INLINE[1] lookup #-}
lookup :: Monad m => Focus a m (Maybe a)
lookup = Focus (pure . (id &&& id))

{-# INLINE[1] delete #-}
delete :: Monad m => Focus a m ()
delete = Focus (const (pure ((), Nothing)))

{-# RULES
  "lookup <* delete" [~1] lookup <* delete = lookupAndDelete
  #-}
lookupAndDelete :: Monad m => Focus a m (Maybe a)
lookupAndDelete = Focus (\ state -> pure (state, Nothing))
