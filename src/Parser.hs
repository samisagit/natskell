{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Word8            as W8

newtype Parser a = Parser {runParser :: BS.ByteString -> Maybe (a, BS.ByteString)}

instance Functor Parser where
  fmap f (Parser x) = Parser $ \s -> do
    (x', s') <- x s
    return (f x', s')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  (Parser f) <*> (Parser x) = Parser $ \s -> do
    (f', s1) <- f s
    (x', s2) <- x s1
    return (f' x', s2)

instance Monad Parser where
  (Parser x) >>= f = Parser $ \s -> do
    (x', s') <- x s
    runParser (f x') s'

instance MonadFail Parser where
  fail _ = Parser $ const Nothing

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
      Just x  -> Just x
      Nothing -> y s

char :: W8.Word8 -> Parser W8.Word8
char c = Parser charP
  where
    charP bs
      | BS.empty == bs = Nothing
      | BS.head bs == c = Just (c, BS.tail bs)
      | otherwise = Nothing

charIn :: [W8.Word8] -> Parser W8.Word8
charIn opts = Parser charP
  where
    charP bs
      | BS.empty == bs = Nothing
      | BS.head bs `elem` opts = Just (BS.head bs, BS.tail bs)
      | otherwise = Nothing

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
      | BS.empty == bs = Nothing
      | W8.isAlphaNum $ BS.head bs = Just (BS.head bs, BS.tail bs)
      | otherwise = Nothing

alphaNumerics :: Parser [W8.Word8]
alphaNumerics = some alphaNumeric

ascii = Parser charP
  where
    charP bs
      | BS.empty == bs = Nothing
      | W8.isAscii $ BS.head bs = Just (BS.head bs, BS.tail bs)
      | otherwise = Nothing

asciis :: Parser [W8.Word8]
asciis = some ascii

not' :: W8.Word8 -> Parser W8.Word8
not' c = Parser charP
  where
    charP bs
      | BS.empty == bs = Nothing
      | BS.head bs /= c = Just (BS.head bs, BS.tail bs)
      | otherwise = Nothing

til :: W8.Word8 -> Parser [W8.Word8]
til = some . not'

integer :: Parser [W8.Word8]
integer = stringWithChars [W8._0 .. W8._9]

take' :: Int -> Parser W8.Word8 -> Parser [W8.Word8]
take' 0 _ = string ""
take' n p = (:) <$> p <*> take' (n -1) p

tokenParser :: Parser [W8.Word8]
tokenParser = alphaNumerics <|> string "*"

subjectParser :: Parser [W8.Word8]
subjectParser = do
  head <- tokenParser
  rest <- ((++) <$> string "." <*> subjectParser) <|> (string ".>" <|> string "")

  return (head ++ rest)

toInt :: BS.ByteString -> Int
toInt bs = read (C.unpack bs) :: Int

