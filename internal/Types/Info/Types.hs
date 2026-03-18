{-# LANGUAGE DeriveGeneric #-}

module Types.Info.Types
  ( Info (..)
  ) where

import qualified Data.ByteString as BS
import           GHC.Generics

data Info = Info
              { server_id     :: BS.ByteString
              , version       :: BS.ByteString
              , go            :: BS.ByteString
              , host          :: BS.ByteString
              , port          :: Int
              , max_payload   :: Int
              , proto         :: Int
              , client_id     :: Maybe Int
              , nonce         :: Maybe BS.ByteString
              , auth_required :: Maybe Bool
              , tls_required  :: Maybe Bool
              , connect_urls  :: Maybe [BS.ByteString]
              , ldm           :: Maybe Bool
              , headers       :: Maybe Bool
              }
  deriving (Eq, Generic, Show)
