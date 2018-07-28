module Focus where

import Focus.Prelude hiding (adjust, update, alter, insert, delete, lookup)


{-|
Abstraction over the modification of an element of a datastructure.

It is composable using the standard typeclasses, e.g.:

>lookupAndDelete :: Monad m => Focus a m (Maybe a)
>lookupAndDelete = lookup <* delete
-}
data Focus element m result = Focus (m (result, Change element)) (element -> m (result, Change element))

deriving instance Functor m => Functor (Focus element m)

instance Monad m => Applicative (Focus element m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Focus element m) where
  return result = Focus (return (result, Leave)) (\ _ -> return (result, Leave))
  (>>=) (Focus aAbsent bPresent) bKleisli = let
    sendSome element = do
      (aResult, aChange) <- bPresent element
      case bKleisli aResult of
        Focus bAbsent bOnElement -> case aChange of
          Leave -> bOnElement element
          Remove -> bAbsent
          Set newElement -> bOnElement newElement
    sendNone = do
      (aResult, aChange) <- aAbsent
      case bKleisli aResult of
        Focus bAbsent bOnElement -> case aChange of
          Set newElement -> bOnElement newElement
          Leave -> bAbsent
          Remove -> bAbsent
    in Focus sendNone sendSome

instance MonadTrans (Focus element) where
  lift m = Focus (fmap (,Leave) m) (const (fmap (,Leave) m))

{-|
What to do with the focused value.

The interpretation of the commands is up to the context APIs.
-}
data Change a =
  Leave {-^ Produce no changes -} |
  Remove {-^ Delete it -} |
  Set a {-^ Replace it with the provided value -}
  deriving (Functor, Eq, Ord, Show)


-- * Pure functions
-------------------------

-- ** Reading functions
-------------------------

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
-}
{-# INLINE member #-}
member :: Monad m => Focus a m Bool
member = fmap (maybe False (const True)) lookup

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:lookup lookup>@.
-}
{-# INLINE[1] lookup #-}
lookup :: Monad m => Focus a m (Maybe a)
lookup = cases (Nothing, Leave) (\ a -> (Just a, Leave))

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:findWithDefault findWithDefault>@
with a better name.
-}
{-# INLINE[1] lookupWithDefault #-}
lookupWithDefault :: Monad m => a -> Focus a m a
lookupWithDefault a = cases (a, Leave) (\ a -> (a, Leave))

-- ** Modifying functions
-------------------------

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:delete delete>@.
-}
{-# INLINE[1] delete #-}
delete :: Monad m => Focus a m ()
delete = unitCases Leave (const Remove)

{-|
Lookup an element and delete it if it exists.

Same as @'lookup' <* 'delete'@.
-}
{-# RULES
  "lookup <* delete" [~1] lookup <* delete = lookupAndDelete
  #-}
{-# INLINE lookupAndDelete #-}
lookupAndDelete :: Monad m => Focus a m (Maybe a)
lookupAndDelete = cases (Nothing, Leave) (\ element -> (Just element, Remove))

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:insert insert>@.
-}
{-# INLINE insert #-}
insert :: Monad m => a -> Focus a m ()
insert a = unitCases (Set a) (const (Set a))

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:insertWith insertWith>@
with a better name.
-}
{-# INLINE insertOrMerge #-}
insertOrMerge :: Monad m => (a -> a -> a) -> a -> Focus a m ()
insertOrMerge merge value = unitCases (Set value) (Set . merge value) 

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:alter alter>@.
-}
{-# INLINE alter #-}
alter :: Monad m => (Maybe a -> Maybe a) -> Focus a m ()
alter fn = unitCases (maybe Leave Set (fn Nothing)) (maybe Leave Set . fn . Just)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:adjust adjust>@.
-}
{-# INLINE adjust #-}
adjust :: Monad m => (a -> a) -> Focus a m ()
adjust fn = unitCases Leave (Set . fn)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:update update>@.
-}
{-# INLINE update #-}
update :: Monad m => (a -> Maybe a) -> Focus a m ()
update fn = unitCases Leave (maybe Leave Set . fn)

-- ** Construction utils
-------------------------

{-|
Lift pure functions which handle the cases of presence and absence of the element.
-}
{-# INLINE cases #-}
cases :: Monad m => (b, Change a) -> (a -> (b, Change a)) -> Focus a m b
cases sendNone sendSome = Focus (return sendNone) (return . sendSome)

{-|
Lift pure functions which handle the cases of presence and absence of the element and produce no result.
-}
{-# INLINE unitCases #-}
unitCases :: Monad m => Change a -> (a -> Change a) -> Focus a m ()
unitCases sendNone sendSome = cases ((), sendNone) (\ a -> ((), sendSome a))


-- * Monadic functions
-------------------------

-- ** Reading functions
-------------------------

{-|
A monadic version of 'lookupWithDefault'.
-}
{-# INLINE[1] lookupWithDefaultM #-}
lookupWithDefaultM :: Monad m => m a -> Focus a m a
lookupWithDefaultM aM = casesM (liftM2 (,) aM (return Leave)) (\ a -> return (a, Leave))

-- ** Modifying functions
-------------------------

{-|
A monadic version of 'insert'.
-}
{-# INLINE insertM #-}
insertM :: Monad m => m a -> Focus a m ()
insertM aM = unitCasesM (fmap Set aM) (const (fmap Set aM))

{-|
A monadic version of 'insertOrMerge'.
-}
{-# INLINE insertOrMergeM #-}
insertOrMergeM :: Monad m => (a -> a -> m a) -> m a -> Focus a m ()
insertOrMergeM merge aM = unitCasesM (fmap Set aM) (\ a' -> aM >>= \ a -> fmap Set (merge a a'))

{-|
A monadic version of 'alter'.
-}
{-# INLINE alterM #-}
alterM :: Monad m => (Maybe a -> m (Maybe a)) -> Focus a m ()
alterM fn = unitCasesM (fmap (maybe Leave Set) (fn Nothing)) (fmap (maybe Leave Set) . fn . Just)

{-|
A monadic version of 'adjust'.
-}
{-# INLINE adjustM #-}
adjustM :: Monad m => (a -> m a) -> Focus a m ()
adjustM fn = updateM (fmap Just . fn)

{-|
A monadic version of 'update'.
-}
{-# INLINE updateM #-}
updateM :: Monad m => (a -> m (Maybe a)) -> Focus a m ()
updateM fn = unitCasesM (return Leave) (fmap (maybe Leave Set) . fn)

-- ** Construction utils
-------------------------

{-|
Lift monadic functions which handle the cases of presence and absence of the element.
-}
{-# INLINE casesM #-}
casesM :: Monad m => m (b, Change a) -> (a -> m (b, Change a)) -> Focus a m b
casesM sendNone sendSome = Focus sendNone sendSome

{-|
Lift monadic functions which handle the cases of presence and absence of the element and produce no result.
-}
{-# INLINE unitCasesM #-}
unitCasesM :: Monad m => m (Change a) -> (a -> m (Change a)) -> Focus a m ()
unitCasesM sendNone sendSome = Focus (fmap ((),) sendNone) (\ a -> fmap ((),) (sendSome a))


-- * Composition
-------------------------

{-|
Map the Focus input.
-}
{-# INLINE mappingInput #-}
mappingInput :: Monad m => (a -> b) -> (b -> a) -> Focus a m x -> Focus b m x
mappingInput aToB bToA (Focus consealA revealA) = Focus consealB revealB where
  consealB = do
    (x, aChange) <- consealA
    return (x, fmap aToB aChange)
  revealB b = do
    (x, aChange) <- revealA (bToA b)
    return (x, fmap aToB aChange)


-- * Change-inspecting functions
-------------------------

{-|
Extends the output with the input.
-}
{-# INLINE extractingInput #-}
extractingInput :: Monad m => Focus a m b -> Focus a m (b, Maybe a)
extractingInput (Focus absent present) =
  Focus newAbsent newPresent
  where
    newAbsent = do
      (b, change) <- absent
      return ((b, Nothing), change)
    newPresent element = do
      (b, change) <- present element
      return ((b, Just element), change)

{-|
Extends the output with the change performed.
-}
{-# INLINE extractingChange #-}
extractingChange :: Monad m => Focus a m b -> Focus a m (b, Change a)
extractingChange (Focus absent present) =
  Focus newAbsent newPresent
  where
    newAbsent = do
      (b, change) <- absent
      return ((b, change), change)
    newPresent element = do
      (b, change) <- present element
      return ((b, change), change)

{-|
Extends the output with a projection on the change that was performed.
-}
{-# INLINE projectingChange #-}
projectingChange :: Monad m => (Change a -> c) -> Focus a m b -> Focus a m (b, c)
projectingChange fn (Focus absent present) =
  Focus newAbsent newPresent
  where
    newAbsent = do
      (b, change) <- absent
      return ((b, fn change), change)
    newPresent element = do
      (b, change) <- present element
      return ((b, fn change), change)

{-|
Extends the output with a flag,
signaling whether a change, which is not 'Leave', has been introduced.
-}
{-# INLINE testingIfModifies #-}
testingIfModifies :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfModifies =
  projectingChange $ \ case
    Leave -> False
    _ -> True

{-|
Extends the output with a flag,
signaling whether the 'Remove' change has been introduced.
-}
{-# INLINE testingIfRemoves #-}
testingIfRemoves :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfRemoves =
  projectingChange $ \ case
    Remove -> True
    _ -> False

{-|
Extends the output with a flag,
signaling whether an item will be inserted.
That is, it didn't exist before and a 'Set' change is introduced.
-}
{-# INLINE testingIfInserts #-}
testingIfInserts :: Monad m => Focus a m b -> Focus a m (b, Bool)
testingIfInserts (Focus absent present) =
  Focus newAbsent newPresent
  where
    newAbsent = do
      (output, change) <- absent
      let testResult = case change of
            Set _ -> True
            _ -> False
          in return ((output, testResult), change)
    newPresent element = do
      (output, change) <- present element
      return ((output, False), change)

{-|
Extend the output with a flag, signaling how the size will be affected by the change.
-}
{-# INLINE testingSizeChange #-}
testingSizeChange :: Monad m => sizeChange {-^ Decreased -} -> sizeChange {-^ Didn't change -} -> sizeChange {-^ Increased -} -> Focus a m b -> Focus a m (b, sizeChange)
testingSizeChange dec none inc (Focus absent present) =
  Focus newAbsent newPresent
  where
    newAbsent = do
      (output, change) <- absent
      let sizeChange = case change of
            Set _ -> inc
            _ -> none
          in return ((output, sizeChange), change)
    newPresent element = do
      (output, change) <- present element
      let sizeChange = case change of
            Remove -> dec
            _ -> none
          in return ((output, sizeChange), change)


-- * STM
-------------------------

{-|
Focus on the contents of a TVar.
-}
{-# INLINE onTVarValue #-}
onTVarValue :: Focus a STM b -> Focus (TVar a) STM b
onTVarValue (Focus concealA presentA) = Focus concealTVar presentTVar where
  concealTVar = concealA >>= traverse interpretAChange where
    interpretAChange = \ case
      Leave -> return Leave
      Set !a -> Set <$> newTVar a
      Remove -> return Leave
  presentTVar var = readTVar var >>= presentA >>= traverse interpretAChange where
    interpretAChange = \ case
      Leave -> return Leave
      Set !a -> writeTVar var a $> Leave
      Remove -> return Remove
