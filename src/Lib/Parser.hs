{-# LANGUAGE OverloadedStrings #-}

module Lib.Parser where

import           Control.Applicative
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Word8            as W8

data ParserErr = ParserErr {message :: String, offset :: Int}
  deriving (Eq, Show)

newtype Parser a = Parser {runParser :: BS.ByteString -> Either ParserErr (a, BS.ByteString) }

instance Functor Parser where
  fmap f (Parser runner) = Parser $ \bs -> do
    (struct, bs') <- runner bs
    return (f struct, bs')

instance Applicative Parser where
  pure x = Parser $ \bs -> Right(x, bs)
  (Parser runnerA) <*> (Parser runnerB) = Parser $ \bs -> do
    (f, bs1) <- runnerA bs
    (struct, bs2) <- runnerB bs1
    return (f struct, bs2)

instance Monad Parser where
  (Parser runner) >>= f = Parser $ \bs -> do
    (struct, bs') <- runner bs
    runParser (f struct) bs'

instance MonadFail Parser where
  fail s = Parser . const $ Left (ParserErr s 0)

instance Alternative Parser where
  some v = s
    where
      m = s <|> pure []
      s = (:) <$> v <*> m
  many v = m
    where
      m = s <|> pure []
      s = (:) <$> v <*> m
  empty = fail ""
  (Parser x) <|> (Parser y) = Parser $ \s ->
    case x s of
      Right x  -> Right x
      Left errA -> do
        case y s of
          Right x   -> Right x
          Left errB -> Left (deepestErr errA errB)

deepestErr :: ParserErr -> ParserErr -> ParserErr
deepestErr a b
  | offset a > offset b = b
  | otherwise = a

char :: W8.Word8 -> Parser W8.Word8
char c = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (ParserErr "nothing to read" 0)
      | BS.head bs == c = Right (c, BS.tail bs)
      | otherwise = Left (ParserErr (errString bs) (BS.length bs))
    errString bs = w8sToString [BS.head bs] ++ " does not match " ++ w8sToString [c] ++ " in " ++ C.unpack bs


charIn :: [W8.Word8] -> Parser W8.Word8
charIn opts = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (ParserErr "nothing to read" 0)
      | BS.head bs `elem` opts = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (ParserErr (errString bs) (BS.length bs))
    errString bs = w8sToString [BS.head bs] ++ " not in " ++ w8sToString opts ++ " in " ++ C.unpack bs

w8sToString :: [W8.Word8] -> String
w8sToString = C.unpack . BS.pack

space :: Parser W8.Word8
space = char W8._space

ss = some space

string :: BS.ByteString -> Parser [W8.Word8]
string bs = mapM char w8s
  where
    w8s = BS.unpack bs

stringWithChars :: [W8.Word8] -> Parser [W8.Word8]
stringWithChars s = some (charIn s)

alphaNumeric = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (ParserErr "nothing to read" (BS.length bs))
      | W8.isAlphaNum $ BS.head bs = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (ParserErr (w8sToString [BS.head bs] ++ " is not alphanumeric in " ++ C.unpack bs) (BS.length bs))

alphaNumerics :: Parser [W8.Word8]
alphaNumerics = some alphaNumeric

ascii = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (ParserErr "nothing to read" (BS.length bs))
      | W8.isAscii $ BS.head bs = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (ParserErr (w8sToString [BS.head bs] ++ " is not ascii in " ++ C.unpack bs) (BS.length bs))

asciis :: Parser [W8.Word8]
asciis = some ascii

not' :: W8.Word8 -> Parser W8.Word8
not' c = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (ParserErr "nothing to read" (BS.length bs))
      | BS.head bs /= c = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (ParserErr (w8sToString [BS.head bs] ++ " was explicitly not allowed by parser in " ++ C.unpack bs) (BS.length bs))

-- TODO: til will read until the end of the input and return successfully if no match is found
-- raised in #93
til :: W8.Word8 -> Parser [W8.Word8]
til = some . not'

digit :: Parser W8.Word8
digit = charIn [W8._0 .. W8._9]

integer :: Parser [W8.Word8]
integer = stringWithChars [W8._0 .. W8._9]

take' :: Int -> Parser W8.Word8 -> Parser [W8.Word8]
take' 0 _ = string ""
take' n p = (:) <$> p <*> take' (n -1) p

tokenParser :: Parser [W8.Word8]
tokenParser = alphaNumerics <|> string "*"

wireTapParser :: Parser [W8.Word8]
wireTapParser = do
  string ">"

specificSubjectParser :: Parser [W8.Word8]
specificSubjectParser = do
  head <- tokenParser
  rest <- ((++) <$> string "." <*> subjectParser) <|> (string ".>" <|> string "")
  return (head ++ rest)

subjectParser :: Parser [W8.Word8]
subjectParser = do
  wireTapParser <|> specificSubjectParser

headersParser :: Int -> Parser [(BS.ByteString, BS.ByteString)]
headersParser 0 = do
  return []
headersParser n = do
  pre <- string "NATS/"
  version <- til W8._cr
  suf <- string "\r\n"
  headerPairsParser (n - sum (map (BS.length . BS.pack) [pre, version, suf]))

headerPairsParser :: Int -> Parser [(BS.ByteString, BS.ByteString)]
headerPairsParser 0 = do
  return []
headerPairsParser n = do
  (key, value, count) <- headerPairParser
  rest <- headerPairsParser (n - count)
  return ((key, value) : rest)

headerPairParser :: Parser (BS.ByteString, BS.ByteString, Int)
headerPairParser = do
  key <- til W8._colon
  char W8._colon
  value <- til W8._cr
  string "\r\n"
  return (toUnspacedBS key, toUnspacedBS value, bslength key + 1 + bslength value + 2)
    where
      toUnspacedBS = BS.pack . stripSpace
      bslength = fromIntegral . BS.length . BS.pack

stripSpace :: [W8.Word8] -> [W8.Word8]
stripSpace [] = []
stripSpace xs = Prelude.takeWhile isNotSpace . Prelude.dropWhile isSpace $ xs
  where
    isSpace = (==) W8._space
    isNotSpace = (/=) W8._space


toInt :: BS.ByteString -> Int
toInt bs = read (C.unpack bs) :: Int

