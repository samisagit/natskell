{-# LANGUAGE OverloadedStrings #-}

module JetStream.Message.Types
  ( AckVerb (..)
  , FetchOption
  , FetchWait (..)
  , Headers
  , Message (..)
  , PullRequest (..)
  , PullResponse (..)
  , PullStatus (..)
  , ackPayload
  , classifyStatusHeaders
  , defaultPullRequest
  , descriptionHeader
  , pullRequest
  , inProgressPayload
  , isStatusMessage
  , nakPayload
  , pullRequestPayload
  , statusHeader
  , termPayload
  , withFetchBatch
  , withFetchWait
  ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (isJust)
import           JetStream.Types
    ( CallOption
    , Headers
    , Payload
    , Subject
    , applyCallOptions
    )

data PullRequest = PullRequest
                     { pullRequestBatch :: Int
                     , pullRequestWait  :: FetchWait
                     }
  deriving (Eq, Show)

data FetchWait = FetchExpiresMicros Int
               | FetchNoWaitMicros Int
  deriving (Eq, Show)

defaultPullRequest :: PullRequest
defaultPullRequest =
  PullRequest
    { pullRequestBatch = 1
    , pullRequestWait = FetchExpiresMicros 1000000
    }

type FetchOption = CallOption PullRequest

pullRequest :: [FetchOption] -> PullRequest
pullRequest options =
  applyCallOptions options defaultPullRequest

withFetchBatch :: Int -> FetchOption
withFetchBatch batch request =
  request { pullRequestBatch = batch }

withFetchWait :: FetchWait -> FetchOption
withFetchWait wait request =
  request { pullRequestWait = wait }

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
      | FetchNoWaitMicros _ <- pullRequestWait request = ",\"no_wait\":true"
      | otherwise = ""
    expiresField =
      case pullRequestWait request of
        FetchNoWaitMicros _ ->
          ""
        FetchExpiresMicros timeoutMicros
          | timeoutMicros <= 0 -> ""
          | otherwise ->
          BS.concat
            [ ",\"expires\":"
            , intBytes (timeoutMicros * 1000)
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
