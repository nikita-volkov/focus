module Focus.Private.Decision where

import Focus.Private.Prelude


-- |
-- A combination of the output value and instruction.
data Decision a b =
  Decision b (Instruction a)
  deriving (Functor)

instance Bifunctor Decision where
  bimap proj1 proj2 (Decision output instruction) =
    Decision (proj2 output) (fmap proj1 instruction)

-- |
-- What to do with the focused value.
-- 
-- The interpretation of the commands is up to the context APIs.
data Instruction a =
  Keep |
  Remove |
  Set a
  deriving (Functor)

