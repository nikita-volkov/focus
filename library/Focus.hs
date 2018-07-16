module Focus where

import Prelude hiding (adjust, update, alter, insert, delete, lookup)
import Control.Applicative
import Control.Arrow
import Control.Monad


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

adjust :: Applicative m => (a -> a) -> Focus a m ()
adjust fn = Focus (pure . ((),) . fmap fn)

update :: Applicative m => (a -> Maybe a) -> Focus a m ()
update fn = Focus (pure . ((),) . flip (>>=) fn)

lookup :: Applicative m => Focus a m (Maybe a)
lookup = Focus (pure . (id &&& id))
