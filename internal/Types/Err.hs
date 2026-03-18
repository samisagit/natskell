module Types.Err
  ( module Types.Err.Types
  , isFatal
  ) where

import           Types.Err.Types

isFatal :: Err -> Bool
isFatal (ErrInvalidSubject _) = False
isFatal (ErrPermViolation _)  = False
isFatal _                     = True
