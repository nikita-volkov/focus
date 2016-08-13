module Focus.Pure
(
  A.Focus(..),
  A.Decision(..),
  A.Instruction(..),
  -- * Implementations of the common patterns
  A.adjust,
  A.update,
  A.alter,
  A.insert,
  A.delete,
  A.lookup,
  A.lookupAndDelete,
)
where

import qualified Focus.Private.Pure as A
