module Focus.Private.Decision where

import Focus.Private.Prelude


-- |
-- A combination of the output value and instruction.
data Decision a b =
  Decision b (Instruction a)
  deriving (Functor, Foldable, Traversable)

instance Bifunctor Decision where
  bimap proj1 proj2 (Decision output instruction) =
    Decision (proj2 output) (fmap proj1 instruction)

instance Bifoldable Decision where
  {-# INLINE bifoldMap #-}
  bifoldMap proj1 proj2 (Decision output instruction) =
    foldMap proj1 instruction <> proj2 output
  
instance Bitraversable Decision where
  {-# INLINE bitraverse #-}
  bitraverse proj1 proj2 (Decision output instruction) =
    Decision <$>
    proj2 output <*>
    traverse proj1 instruction

-- |
-- What to do with the focused value.
-- 
-- The interpretation of the commands is up to the context APIs.
data Instruction a =
  Keep |
  Remove |
  Set a
  deriving (Functor, Foldable, Traversable)

instructionToMaybe :: Instruction a -> Maybe a
instructionToMaybe =
  \case
    Set a -> Just a
    _ -> Nothing
