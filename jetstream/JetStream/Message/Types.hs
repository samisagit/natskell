{-# LANGUAGE OverloadedStrings #-}

module JetStream.Message.Types
  ( AckVerb (..)
  , Headers
  , Message (..)
  , PullRequest (..)
  , PullResponse (..)
  , PullStatus (..)
  , ackPayload
  , classifyStatusHeaders
  , defaultPullRequest
  , descriptionHeader
  , inProgressPayload
  , isStatusMessage
  , nakPayload
  , pullRequestPayload
  , statusHeader
  , termPayload
  ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (isJust)
import           JetStream.Types       (Headers, Payload, Subject)

data PullRequest = PullRequest
                     { pullRequestBatch         :: Int
                     , pullRequestTimeoutMicros :: Int
                     , pullRequestNoWait        :: Bool
                     }
  deriving (Eq, Show)

defaultPullRequest :: PullRequest
defaultPullRequest =
  PullRequest
    { pullRequestBatch = 1
    , pullRequestTimeoutMicros = 1000000
    , pullRequestNoWait = False
    }

data PullResponse = PullResponse
                      { pullResponseMessages :: [Message]
                      , pullResponseStatus   :: Maybe PullStatus
                      }
  deriving (Eq, Show)

data Message = Message
                 { messageSubject :: Subject
                 , messagePayload :: Maybe Payload
                 , messageHeaders :: Maybe Headers
                 , messageReplyTo :: Maybe Subject
                 , messageStatus  :: Maybe PullStatus
                 }
  deriving (Eq, Show)

data PullStatus = PullNoMessages (Maybe ByteString)
                | PullRequestTimeout (Maybe ByteString)
                | PullStatusError ByteString (Maybe ByteString)
  deriving (Eq, Show)

data AckVerb = Ack | Nak | InProgress | Term
  deriving (Eq, Show)

ackPayload :: AckVerb -> Payload
ackPayload Ack        = "+ACK"
ackPayload Nak        = "-NAK"
ackPayload InProgress = "+WPI"
ackPayload Term       = "+TERM"

nakPayload :: Payload
nakPayload = ackPayload Nak

inProgressPayload :: Payload
inProgressPayload = ackPayload InProgress

termPayload :: Payload
termPayload = ackPayload Term

statusHeader :: Headers -> Maybe ByteString
statusHeader = lookupHeader "Status"

descriptionHeader :: Headers -> Maybe ByteString
descriptionHeader = lookupHeader "Description"

classifyStatusHeaders :: Maybe Headers -> Maybe PullStatus
classifyStatusHeaders Nothing = Nothing
classifyStatusHeaders (Just headers) =
  case statusHeader headers of
    Nothing     -> Nothing
    Just "404"  -> Just (PullNoMessages description)
    Just "408"  -> Just (PullRequestTimeout description)
    Just status -> Just (PullStatusError status description)
  where
    description = descriptionHeader headers

isStatusMessage :: Message -> Bool
isStatusMessage = isJust . messageStatus

pullRequestPayload :: Int -> PullRequest -> Payload
pullRequestPayload requestedBatch request =
  BS.concat
    [ "{\"batch\":"
    , intBytes (max 1 requestedBatch)
    , noWaitField
    , expiresField
    , "}"
    ]
  where
    noWaitField
      | pullRequestNoWait request = ",\"no_wait\":true"
      | otherwise = ""
    expiresField
      | pullRequestNoWait request = ""
      | pullRequestTimeoutMicros request <= 0 = ""
      | otherwise =
          BS.concat
            [ ",\"expires\":"
            , intBytes (pullRequestTimeoutMicros request * 1000)
            ]

lookupHeader :: ByteString -> Headers -> Maybe ByteString
lookupHeader needle = go
  where
    normalizedNeedle = normalizeHeaderName needle
    go [] = Nothing
    go ((name, value):rest)
      | normalizeHeaderName name == normalizedNeedle = Just value
      | otherwise = go rest

normalizeHeaderName :: ByteString -> ByteString
normalizeHeaderName = BC.map toLower

intBytes :: Int -> ByteString
intBytes = BC.pack . show
