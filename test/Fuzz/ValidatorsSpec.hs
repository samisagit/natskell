{-# LANGUAGE OverloadedStrings #-}

module ValidatorsSpec (spec) where

import qualified Data.ByteString       as BS
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
    Just _  -> not (connectRules c)
    Nothing -> connectRules c

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
  | otherwise = True


propPub :: Pub -> Bool
propPub p =
  case validate p of
    Just _  -> not (pubRules p)
    Nothing -> pubRules p

pubRules :: Pub -> Bool
pubRules p
  | Types.Pub.subject p == "" = False
  | Types.Pub.replyTo p == Just "" = False
  | Types.Pub.payload p == Just "" = False
  | otherwise = True

propSub :: Sub -> Bool
propSub s =
  case validate s of
    Just _  -> not (subRules s)
    Nothing -> subRules s

subRules :: Sub -> Bool
subRules s
  | Types.Sub.subject s == "" = False
  | Types.Sub.queueGroup s == Just "" = False
  | Types.Sub.sid s == "" = False
  | otherwise = True

propUnsub :: Unsub -> Bool
propUnsub u =
  case validate u of
    Just _  -> not (unsubRules u)
    Nothing -> unsubRules u

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

instance Arbitrary Pub where
   arbitrary = Pub <$> arbitrary
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

