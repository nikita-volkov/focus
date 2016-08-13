module Focus.Impure
(
  A.Focus(..),
  B.Decision(..),
  B.Instruction(..),
  -- * Implementations of the common patterns
  A.adjust,
  A.update,
  A.alter,
  A.insert,
  A.delete,
  A.lookup,
)
where

import qualified Focus.Private.Impure as A
import qualified Focus.Private.Decision as B
