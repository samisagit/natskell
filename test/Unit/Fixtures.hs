{-# LANGUAGE OverloadedStrings #-}

module Fixtures where

import qualified Data.ByteString as BS

versionCases :: [BS.ByteString]
versionCases = ["0.0.0", "v1.0.1", "13.0.0+123"]

boolCases = [True, False]

userCases :: [BS.ByteString]
userCases = ["samisagit", "sam@google.com"]

passCases :: [BS.ByteString]
passCases = ["dsalkj09898(*)(UHJHI&*&*)(910"]

nameCases :: [BS.ByteString]
nameCases = ["natskell-client"]

sigCases :: [BS.ByteString]
sigCases = ["eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQSflKxwRJSMeKKF2QT4fwpMeJf36POk6yJVadQssw5c"]

jwtCases :: [BS.ByteString]
jwtCases = ["eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"]

goVersionCases :: [BS.ByteString]
goVersionCases = ["1.17"]

hostCases :: [BS.ByteString]
hostCases = ["192.168.1.7"]

serverIDCases :: [BS.ByteString]
serverIDCases = ["123"]

portCases :: [Int]
portCases = [4222]
maxPayloadCases :: [Int]
maxPayloadCases = [0, 512, 1024]
protocolCases :: [Int]
protocolCases = [1,2,3]
clientIDCases :: [Int]
clientIDCases = [1, 100]

sidCases :: [BS.ByteString]
sidCases = ["1", "100", "abc", "abc123"]

connectStringCases :: [[BS.ByteString]]
connectStringCases = [[], ["127.0.0.1"], ["127.0.0.1:4222", "0.0.0.0:4222"]]

subjectCases :: [BS.ByteString]
subjectCases = ["FOO", "FOO.BAR", "FOO.BAR.>", ">", "foo.bar", "123.456", "FOO.*.BAR", "FOO.*.BAR.*.>"]

invalidSubjectCases :: [BS.ByteString]
invalidSubjectCases = [" FOO", "FOO>BAR", "FOO ", "F OO", "FOO.**"]

payloadCases :: [(Int, Maybe BS.ByteString)]
payloadCases = [(12, Just "some payload"), (24, Just "some\r\nmulti line payload"), (41, Just ".*some payload ** with specials chars > *"), (0, Nothing)]

headerCases :: [[(BS.ByteString, BS.ByteString)]]
headerCases = [[("header", "abc")], [("header", "abc"), ("HEADER", "123")]]

maybeify :: [a] -> [Maybe a]
maybeify xs = Nothing : fmap Just xs

