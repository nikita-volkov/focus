module Focus.Private.Impure
where

import Focus.Private.Prelude hiding (adjust, update, alter, insert, delete, lookup, Const)
import qualified Focus.Private.Pure as A


data Focus a m b =
  Const (m (A.Decision a b)) |
  Lookup (Maybe a -> m (A.Decision a b))
  deriving (Functor)


{-# INLINE liftPure #-}
liftPure :: Monad m => A.Focus a b -> Focus a m b
liftPure =
  \case
    A.Const output ->
      Const (return output)
    A.Lookup lookupFn ->
      Lookup (return . lookupFn)

{-# INLINE mapDecision #-}
mapDecision :: Monad m => (A.Decision a b1 -> A.Decision a b2) -> Focus a m b1 -> Focus a m b2
mapDecision mapping =
  \case
    Const fx ->
      Const (liftM mapping fx)
    Lookup fxFn ->
      Lookup (liftM mapping . fxFn)

{-# INLINE mapInput #-}
mapInput :: Monad m => (a1 -> a2) -> (a2 -> a1) -> Focus a2 m b -> Focus a1 m b
mapInput proj1 proj2 =
  \case
    Const fx ->
      Const (fmap (second (fmap proj2)) fx)
    Lookup fxFn ->
      Lookup (fmap (second (fmap proj2)) . fxFn . fmap proj1)

-- |
-- Projection to a Lookup function.
{-# INLINE toLookupFn #-}
toLookupFn :: Focus a m b -> (Maybe a -> m (A.Decision a b))
toLookupFn =
  \case
    Const fx ->
      const fx
    Lookup fxFn ->
      fxFn


-- * Instruction extraction
-------------------------

{-# INLINE extractingInstruction #-}
extractingInstruction :: Monad m => Focus a m b -> Focus a m (b, A.Instruction a)
extractingInstruction =
  mapDecision $
  \ (output, instruction) -> ((output, instruction), instruction)

{-# INLINE projectingInstruction #-}
projectingInstruction :: Monad m => (A.Instruction a -> c) -> Focus a m b -> Focus a m (b, c)
projectingInstruction fn =
  mapDecision $
  \ (output, instruction) -> ((output, fn instruction), instruction)

-- |
-- Extends the output with a flag,
-- saying whether an instruction, which is not 'Keep', has been produced.
{-# INLINE testingIfModifies #-}
testingIfModifies :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfModifies =
  projectingInstruction $
  \case
    A.Keep -> False
    _ -> True

-- |
-- Extends the output with a flag,
-- saying whether the 'Remove' instruction has been produced.
{-# INLINE testingIfRemoves #-}
testingIfRemoves :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfRemoves =
  projectingInstruction $
  \case
    A.Remove -> True
    _ -> False

-- |
-- Extends the output with a flag,
-- saying whether an item will be inserted.
-- That is, it didn't exist before and a Set instruction is produced.
{-# INLINE testingIfInserts #-}
testingIfInserts :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfInserts =
  Lookup . lookupFn . toLookupFn
  where
    lookupFn fxFn lookupResult =
      do
        (output, instruction) <- fxFn lookupResult
        return ((output, isNothing lookupResult && instructionIsReplace instruction), instruction)
    instructionIsReplace =
      \case
        A.Set _ -> True
        _ -> False


-- * Implementations of the common patterns
-------------------------

-- |
-- A monadic version of 'Focus.Pure.adjust'.
{-# INLINE adjust #-}
adjust :: (Monad m) => (a -> m a) -> Focus a m ()
adjust f = 
  Lookup (maybe (return ((), A.Keep)) (liftM (((),) . A.Set) . f))

-- |
-- A monadic version of 'Focus.Pure.update'.
{-# INLINE update #-}
update :: (Monad m) => (a -> m (Maybe a)) -> Focus a m ()
update f =
  Lookup (maybe (return ((), A.Keep)) (liftM (((),) . maybe A.Remove A.Set) . f))

-- |
-- A monadic version of 'Focus.Pure.alter'.
{-# INLINE alter #-}
alter :: (Monad m) => (Maybe a -> m (Maybe a)) -> Focus a m ()
alter f =
  Lookup (liftM (((),) . maybe A.Remove A.Set) . f)

-- |
-- A monadic version of 'Focus.Pure.insert'.
{-# INLINE insert #-}
insert :: (Monad m) => a -> Focus a m ()
insert a =
  Const (return ((), A.Set a))

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

