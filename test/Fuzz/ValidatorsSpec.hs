{-# LANGUAGE OverloadedStrings #-}

module ValidatorsSpec (spec) where

import qualified Data.ByteString       as BS
import           Data.Maybe
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import           Types.Connect
import           Types.Pub
import           Types.Sub
import           Types.Unsub
import           Validators.Validators

spec :: Spec
spec = do
  qc

qc = do
  describe "validator" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check given connect" . property $
        propConnect
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check given pub" . property $
        propPub
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check given sub" . property $
        propSub
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check given unsub" . property $
        propUnsub

propConnect :: Connect -> Bool
propConnect c =
  case validate c of
    Left _  -> not (connectRules c)
    Right _ -> connectRules c

connectRules :: Connect -> Bool
connectRules c
  | auth_token c == Just "" = False
  | user c == Just "" = False
  | pass c == Just "" = False
  | name c == Just "" = False
  | lang c ==  "" = False
  | version c == "" = False
  | protocol c `notElem` [Nothing, Just 0, Just 1] = False
  | sig c == Just "" = False
  | jwt c == Just "" = False
  | nkey c == Just "" = False
  | otherwise = True


propPub :: Pub -> Bool
propPub p =
  case validate p of
    Left _  -> not (pubRules p)
    Right _ -> pubRules p

pubRules :: Pub -> Bool
pubRules p
  | Types.Pub.subject p == "" = False
  | Types.Pub.replyTo p == Just "" = False
  | Types.Pub.payload p == Just "" = False
  | headers == Just [] = False
  | isJust headers && Prelude.any (\(k, _) -> k == "") (fromJust headers) = False
  | isJust headers && Prelude.any (\(_, v) -> v == "") (fromJust headers) = False
  | otherwise = True
  where
    headers = Types.Pub.headers p

propSub :: Sub -> Bool
propSub s =
  case validate s of
    Left _  -> not (subRules s)
    Right _ -> subRules s

subRules :: Sub -> Bool
subRules s
  | Types.Sub.subject s == "" = False
  | Types.Sub.queueGroup s == Just "" = False
  | Types.Sub.sid s == "" = False
  | otherwise = True

propUnsub :: Unsub -> Bool
propUnsub u =
  case validate u of
    Left _  -> not (unsubRules u)
    Right _ -> unsubRules u

unsubRules :: Unsub -> Bool
unsubRules u
  | Types.Unsub.sid u == "" = False
  | otherwise = True

instance Arbitrary Connect where
   arbitrary = Connect <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance Arbitrary Pub where
   arbitrary = Pub <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary Sub where
   arbitrary = Sub <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance Arbitrary Unsub where
   arbitrary = Unsub <$> arbitrary
                     <*> arbitrary

instance Arbitrary BS.ByteString where arbitrary = BS.pack <$> arbitrary
