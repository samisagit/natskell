{-# LANGUAGE OverloadedStrings #-}

module Validators.Validators where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isSpace)

class Validator a where
  validate :: a -> Either ByteString ()

validateSubjectSyntax :: ByteString -> Either ByteString ()
validateSubjectSyntax subject
  | BC.any isSpace subject =
      Left "subject contains whitespace"
  | any BS.null tokens =
      Left "subject contains empty token"
  | otherwise =
      validateWildcardTokens tokens
  where
    tokens = BC.split '.' subject

validateQueueGroupSyntax :: ByteString -> Either ByteString ()
validateQueueGroupSyntax queueGroup
  | BC.any isSpace queueGroup =
      Left "queue group contains whitespace"
  | otherwise =
      Right ()

validateHeaderKeySyntax :: ByteString -> Either ByteString ()
validateHeaderKeySyntax key
  | ":" `BS.isInfixOf` key =
      Left "header key contains colon"
  | hasLineBreak key =
      Left "header key contains line break"
  | otherwise =
      Right ()

validateHeaderValueSyntax :: ByteString -> Either ByteString ()
validateHeaderValueSyntax value
  | hasLineBreak value =
      Left "header value contains line break"
  | otherwise =
      Right ()

validateWildcardTokens :: [ByteString] -> Either ByteString ()
validateWildcardTokens [] =
  Right ()
validateWildcardTokens [">"] =
  Right ()
validateWildcardTokens (">":_) =
  Left "full wildcard must be the final token"
validateWildcardTokens (token:rest)
  | token == "*" =
      validateWildcardTokens rest
  | ">" `BS.isInfixOf` token =
      Left "full wildcard must be a standalone token"
  | "*" `BS.isInfixOf` token =
      Left "partial wildcard must be a standalone token"
  | otherwise =
      validateWildcardTokens rest

hasLineBreak :: ByteString -> Bool
hasLineBreak bytes =
  "\r" `BS.isInfixOf` bytes || "\n" `BS.isInfixOf` bytes
