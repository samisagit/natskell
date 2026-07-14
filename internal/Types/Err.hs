module Types.Err
  ( Reason
  , Err (..)
  , errReason
  , isFatal
  ) where

import           Data.ByteString (ByteString)

type Reason = ByteString

data Err = ErrUnknownOp Reason
         | ErrRoutePortConn Reason
         | ErrAuthViolation Reason
         | ErrAuthTimeout Reason
         | ErrAuthExpired Reason
         | ErrAuthRevoked Reason
         | ErrAccountAuthExpired Reason
         | ErrInvalidProtocol Reason
         | ErrMaxControlLineEx Reason
         | ErrErr Reason
         | ErrTlsRequired Reason
         | ErrStaleConn Reason
         | ErrMaxConnsEx Reason
         | ErrSlowConsumer Reason
         | ErrMaxPayload Reason
         | ErrInvalidSubject Reason
         | ErrPermViolation Reason
  deriving (Eq, Show)

isFatal :: Err -> Bool
isFatal (ErrInvalidSubject _) = False
isFatal (ErrPermViolation _)  = False
isFatal _                     = True

errReason :: Err -> Reason
errReason err =
  case err of
    ErrUnknownOp reason          -> reason
    ErrRoutePortConn reason      -> reason
    ErrAuthViolation reason      -> reason
    ErrAuthTimeout reason        -> reason
    ErrAuthExpired reason        -> reason
    ErrAuthRevoked reason        -> reason
    ErrAccountAuthExpired reason -> reason
    ErrInvalidProtocol reason    -> reason
    ErrMaxControlLineEx reason   -> reason
    ErrErr reason                -> reason
    ErrTlsRequired reason        -> reason
    ErrStaleConn reason          -> reason
    ErrMaxConnsEx reason         -> reason
    ErrSlowConsumer reason       -> reason
    ErrMaxPayload reason         -> reason
    ErrInvalidSubject reason     -> reason
    ErrPermViolation reason      -> reason
