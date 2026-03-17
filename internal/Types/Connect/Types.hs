{-# LANGUAGE DeriveGeneric #-}

module Types.Connect.Types
  ( Connect (..)
  ) where

import qualified Data.ByteString as BS
import           GHC.Generics

data Connect = Connect
                 { verbose       :: Bool
                 , pedantic      :: Bool
                 , tls_required  :: Bool
                 , auth_token    :: Maybe BS.ByteString
                 , user          :: Maybe BS.ByteString
                 , pass          :: Maybe BS.ByteString
                 , name          :: Maybe BS.ByteString
                 , lang          :: BS.ByteString
                 , version       :: BS.ByteString
                 , protocol      :: Maybe Int
                 , echo          :: Maybe Bool
                 , sig           :: Maybe BS.ByteString
                 , jwt           :: Maybe BS.ByteString
                 , nkey          :: Maybe BS.ByteString
                 , no_responders :: Maybe Bool
                 , headers       :: Maybe Bool
                 }
  deriving (Eq, Generic, Show)
