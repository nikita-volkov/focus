module Focus.Pure
(
  A.Focus(..),
  A.Decision(..),
  -- * Implementations of the common patterns
  A.adjust,
  A.update,
  A.alter,
  A.insert,
  A.delete,
  A.lookup,
)
where

import qualified Focus.Private.Pure as A
