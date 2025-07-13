module Types.Err where

import           Data.ByteString

type Reason = ByteString

data Err = ErrUnknownOp Reason
         | ErrRoutePortConn Reason
         | ErrAuthViolation Reason
         | ErrAuthTimeout Reason
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
isFatal (ErrPermViolation _) = False
isFatal _ = True

