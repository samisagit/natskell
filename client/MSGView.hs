module MSGView where

import qualified Data.ByteString as BS
import qualified Types.Msg       as M

-- | MsgView represents a MSG in the NATS protocol.
data MsgView = MsgView
                 { -- | The subject of the message.
                   subject :: BS.ByteString
                   -- | The SID (subscription ID) of the message.
                 , sid     :: BS.ByteString
                   -- | The replyTo subject, if any.
                 , replyTo :: Maybe BS.ByteString
                   -- | The payload of the message, if any.
                 , payload :: Maybe BS.ByteString
                   -- | Headers associated with the message, if any.
                 , headers :: Maybe [(BS.ByteString, BS.ByteString)]
                 }
  deriving (Eq, Show)

transformMsg :: M.Msg -> MsgView
transformMsg msg = MsgView {
    subject = M.subject msg,
    sid     = M.sid msg,
    replyTo = M.replyTo msg,
    payload = M.payload msg,
    headers = M.headers msg
  }

