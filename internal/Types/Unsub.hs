{-# LANGUAGE OverloadedStrings #-}

module Types.Unsub where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Transformers.Transformers (Transformer (..))
import           Types.Msg                 (SID)
import           Validators.Validators

data Unsub = Unsub
               { sid    :: SID
               , maxMsg :: Maybe Int
               }
  deriving (Eq, Show)

instance Transformer Unsub where
  transform unsubMsg =
    case maxMsg unsubMsg of
      Just count -> LBS.fromChunks ["UNSUB ", sid unsubMsg, " ", BC.pack (show count), "\r\n"]
      Nothing -> LBS.fromChunks ["UNSUB ", sid unsubMsg, "\r\n"]

instance Validator Unsub where
  validate u = do
    validateSid u

validateSid :: Unsub -> Either ByteString ()
validateSid u
  | sid u == "" = Left "explicit empty sid"
  | otherwise = Right ()
