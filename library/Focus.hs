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

{-|
What to do with the focused value.

The interpretation of the commands is up to the context APIs.
-}
data Change a = Leave | Remove | Set a deriving (Functor, Eq, Ord, Show)


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
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:findWithDefault findWithDefault>@
with a better name.
-}
{-# INLINE[1] lookupWithDefaultM #-}
lookupWithDefaultM :: Monad m => m a -> Focus a m a
lookupWithDefaultM aM = casesM (liftM2 (,) aM (return Leave)) (\ a -> return (a, Leave))

-- ** Modifying functions
-------------------------

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:insert insert>@.
-}
{-# INLINE insertM #-}
insertM :: Monad m => m a -> Focus a m ()
insertM aM = unitCasesM (fmap Set aM) (const (fmap Set aM))

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:insertWith insertWith>@
with a better name.
-}
{-# INLINE insertOrMergeM #-}
insertOrMergeM :: Monad m => (a -> a -> m a) -> m a -> Focus a m ()
insertOrMergeM merge aM = unitCasesM (fmap Set aM) (\ a' -> aM >>= \ a -> fmap Set (merge a a'))

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:alter alter>@.
-}
{-# INLINE alterM #-}
alterM :: Monad m => (Maybe a -> m (Maybe a)) -> Focus a m ()
alterM fn = unitCasesM (fmap (maybe Leave Set) (fn Nothing)) (fmap (maybe Leave Set) . fn . Just)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:adjust adjust>@.
-}
{-# INLINE adjustM #-}
adjustM :: Monad m => (a -> m a) -> Focus a m ()
adjustM fn = updateM (fmap Just . fn)

{-|
Reproduces the behaviour of
@Data.Map.<http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Lazy.html#v:update update>@.
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
