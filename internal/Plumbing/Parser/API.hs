module Parser.API
  ( ParseStep (..)
  , ParsedMessage (..)
  , ParserAPI (..)
  ) where

import           Data.ByteString (ByteString)
import           Types.Err       (Err)
import           Types.Info      (Info)
import           Types.Msg       (Msg)
import           Types.Ok        (Ok)
import           Types.Ping      (Ping)
import           Types.Pong      (Pong)

data ParseStep a = Emit a ByteString
                 | NeedMore
                 | DropPrefix Int String
                 | Reject String
  deriving (Eq, Show)

data ParsedMessage = ParsedPing Ping
                   | ParsedPong Pong
                   | ParsedOk Ok
                   | ParsedErr Err
                   | ParsedInfo Info
                   | ParsedMsg Msg
  deriving (Eq, Show)

newtype ParserAPI a = ParserAPI { parse :: ByteString -> ParseStep a }
