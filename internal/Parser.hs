{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( module Parser.Types
  , offset
  , deepestErr
  , solveErr
  , char
  , charIn
  , w8sToString
  , space
  , ss
  , spaces1
  , string
  , stringBS
  , stringWithChars
  , alphaNumeric
  , alphaNumerics
  , alphaNumericsBS
  , subjectTokenChar
  , ascii
  , asciis
  , anyByte
  , not'
  , til
  , takeTill1
  , digit
  , integer
  , integerBS
  , take'
  , takeBytes
  , tokenParser
  , wireTapParser
  , specificSubjectParser
  , subjectParser
  , subjectParserBS
  , headersParser
  , headerPairsParser
  , headerPairParser
  , stripSpace
  , stripSpaceBS
  , spaceChar
  , tabChar
  , isWhitespace
  , isSubjectTokenChar
  , charToWord8
  , toInt
  , toIntW8
  , toIntBS
  ) where

import           Control.Applicative
import           Control.Monad.Fail    (MonadFail (..))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           Data.Char             (ord)
import           Data.List             (foldl')
import qualified Data.Word8            as W8
import           Parser.Types
import           Prelude               hiding (fail)

offset :: ParserErr -> Int
offset (UnexpectedEndOfInput _ o) = o
offset (UnexpectedChar _ o)       = o

instance Functor Parser where
  fmap f (Parser runner) = Parser $ \bs -> do
    (struct, bs') <- runner bs
    return (f struct, bs')

instance Applicative Parser where
  pure x = Parser $ \bs -> Right (x, bs)
  (Parser runnerA) <*> (Parser runnerB) = Parser $ \bs -> do
    (f, bs1) <- runnerA bs
    (struct, bs2) <- runnerB bs1
    return (f struct, bs2)

instance Monad Parser where
  (Parser runner) >>= f = Parser $ \bs -> do
    (struct, bs') <- runner bs
    runParser (f struct) bs'

instance MonadFail Parser where
  fail s = Parser . const $ Left (UnexpectedEndOfInput s 0)

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

solveErr :: ParserErr -> Int -> Suggestion
solveErr (UnexpectedEndOfInput _ _) _    = SuggestPull -- This suggests we pull more data to satisfy the parser.
solveErr (UnexpectedChar r depth) length = SuggestDrop ((length - depth)+1) r   -- This suggests we drop the invalid prefix.

char :: W8.Word8 -> Parser W8.Word8
char c = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (UnexpectedEndOfInput "nothing to read" 0)
      | BS.head bs == c = Right (c, BS.tail bs)
      | otherwise = Left (UnexpectedChar (errString bs) (BS.length bs))
    errString bs = w8sToString [BS.head bs] ++ " does not match " ++ w8sToString [c] ++ " in " ++ C.unpack bs

charIn :: [W8.Word8] -> Parser W8.Word8
charIn allowedBytes = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (UnexpectedEndOfInput "nothing to read" 0)
      | BS.head bs `elem` allowedBytes = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (UnexpectedChar (errString bs) (BS.length bs))
    errString bs = w8sToString [BS.head bs] ++ " not in " ++ w8sToString allowedBytes ++ " in " ++ C.unpack bs

w8sToString :: [W8.Word8] -> String
w8sToString = C.unpack . BS.pack

space :: Parser W8.Word8
space = charIn [spaceChar, tabChar]

ss = some space

spaces1 :: Parser ()
spaces1 = Parser $ \bs ->
  case BS.span isWhitespace bs of
    (spaces, rest)
      | BS.null spaces ->
        if BS.null bs
          then Left (UnexpectedEndOfInput "nothing to read" 0)
          else Left (UnexpectedChar (errString bs) (BS.length bs))
      | otherwise -> Right ((), rest)
  where
    errString bs =
      w8sToString [BS.head bs]
        ++ " not in "
        ++ w8sToString [spaceChar, tabChar]
        ++ " in "
        ++ C.unpack bs

string :: BS.ByteString -> Parser [W8.Word8]
string bs = mapM char w8s
  where
    w8s = BS.unpack bs

stringBS :: BS.ByteString -> Parser BS.ByteString
stringBS target = BS.pack <$> string target

stringWithChars :: [W8.Word8] -> Parser [W8.Word8]
stringWithChars s = some (charIn s)

alphaNumeric = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (UnexpectedEndOfInput "nothing to read" (BS.length bs))
      | W8.isAlphaNum $ BS.head bs = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (UnexpectedChar (w8sToString [BS.head bs] ++ " is not alphanumeric in " ++ C.unpack bs) (BS.length bs))

alphaNumerics :: Parser [W8.Word8]
alphaNumerics = some alphaNumeric

alphaNumericsBS :: Parser BS.ByteString
alphaNumericsBS = BS.pack <$> alphaNumerics

subjectTokenChar :: Parser W8.Word8
subjectTokenChar = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (UnexpectedEndOfInput "nothing to read" (BS.length bs))
      | isSubjectTokenChar (BS.head bs) = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (UnexpectedChar (w8sToString [BS.head bs] ++ " is not a valid subject token char in " ++ C.unpack bs) (BS.length bs))

ascii = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (UnexpectedEndOfInput "nothing to read" (BS.length bs))
      | W8.isAscii $ BS.head bs = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (UnexpectedChar (w8sToString [BS.head bs] ++ " is not ascii in " ++ C.unpack bs) (BS.length bs))

asciis :: Parser [W8.Word8]
asciis = some ascii

anyByte :: Parser W8.Word8
anyByte = Parser $ \bs ->
  case BS.uncons bs of
    Nothing -> Left (UnexpectedEndOfInput "nothing to read" (BS.length bs))
    Just (byte, rest) -> Right (byte, rest)

not' :: W8.Word8 -> Parser W8.Word8
not' c = Parser charP
  where
    charP bs
      | BS.empty == bs = Left (UnexpectedEndOfInput "nothing to read" (BS.length bs))
      | BS.head bs /= c = Right (BS.head bs, BS.tail bs)
      | otherwise = Left (UnexpectedChar (w8sToString [BS.head bs] ++ " was explicitly not allowed by parser in " ++ C.unpack bs) (BS.length bs))

til :: W8.Word8 -> Parser [W8.Word8]
til d = Parser $ \x -> do
  if BS.elem d x then runParser (some (not' d)) x
    else Left ( UnexpectedEndOfInput "unexpected end of input" (BS.length x))

takeTill1 :: W8.Word8 -> Parser BS.ByteString
takeTill1 d = BS.pack <$> til d

digit :: Parser W8.Word8
digit = charIn [W8._0 .. W8._9]

integer :: Parser [W8.Word8]
integer = stringWithChars [W8._0 .. W8._9]

integerBS :: Parser BS.ByteString
integerBS = BS.pack <$> integer

take' :: Int -> Parser W8.Word8 -> Parser [W8.Word8]
take' 0 _ = string ""
take' n p = (:) <$> p <*> take' (n -1) p

takeBytes :: Int -> Parser BS.ByteString
takeBytes n = BS.pack <$> take' n anyByte

tokenParser :: Parser [W8.Word8]
tokenParser = string "*" <|> some subjectTokenChar

wireTapParser :: Parser [W8.Word8]
wireTapParser = do
  string ">"

specificSubjectParser :: Parser [W8.Word8]
specificSubjectParser = do
  head <- tokenParser
  rest <- ((++) <$> string "." <*> subjectParser) <|> (ss >> return [])
  return (head ++ rest)

subjectParser :: Parser [W8.Word8]
subjectParser = (wireTapParser <* ss) <|> specificSubjectParser

subjectParserBS :: Parser BS.ByteString
subjectParserBS = BS.pack <$> subjectParser

headersParser :: Int -> Parser [(BS.ByteString, BS.ByteString)]
headersParser 0 = do
  return []
headersParser n = do
  _ <- stringBS "NATS/"
  version <- takeTill1 W8._cr
  _ <- stringBS "\r\n"
  headerPairsParser (n - (BS.length "NATS/" + BS.length version + 2))

headerPairsParser :: Int -> Parser [(BS.ByteString, BS.ByteString)]
headerPairsParser 0 = do
  return []
headerPairsParser n = do
  (key, value, count) <- headerPairParser
  rest <- headerPairsParser (n - count)
  return ((key, value) : rest)

headerPairParser :: Parser (BS.ByteString, BS.ByteString, Int)
headerPairParser = do
  key <- takeTill1 W8._colon
  char W8._colon
  value <- takeTill1 W8._cr
  _ <- stringBS "\r\n"
  return (toUnspacedBS key, toUnspacedBS value, BS.length key + 1 + BS.length value + 2)
    where
      toUnspacedBS = stripSpaceBS

stripSpace :: [W8.Word8] -> [W8.Word8]
stripSpace [] = []
stripSpace xs = Prelude.takeWhile isNotSpace . Prelude.dropWhile isSpace $ xs
  where
    isSpace = isWhitespace
    isNotSpace = not . isWhitespace

stripSpaceBS :: BS.ByteString -> BS.ByteString
stripSpaceBS =
  BS.dropWhile isWhitespace . dropWhileEndBS isWhitespace

dropWhileEndBS :: (W8.Word8 -> Bool) -> BS.ByteString -> BS.ByteString
dropWhileEndBS predicate =
  BS.reverse . BS.dropWhile predicate . BS.reverse

spaceChar :: W8.Word8
spaceChar = charToWord8 ' '

tabChar :: W8.Word8
tabChar = charToWord8 '\t'

isWhitespace :: W8.Word8 -> Bool
isWhitespace w = w == spaceChar || w == tabChar

isSubjectTokenChar :: W8.Word8 -> Bool
isSubjectTokenChar w =
  w /= spaceChar
    && w /= tabChar
    && w /= charToWord8 '.'
    && w /= charToWord8 '>'
    && w /= charToWord8 '*'
    && w /= charToWord8 '\r'
    && w /= charToWord8 '\n'

charToWord8 :: Char -> W8.Word8
charToWord8 = fromIntegral . ord


toInt :: BS.ByteString -> Int
toInt bs = read (C.unpack bs) :: Int

toIntW8 :: [W8.Word8] -> Int
toIntW8 = foldl' step 0
  where
    step acc w = acc * 10 + (fromIntegral w - fromIntegral W8._0)

toIntBS :: BS.ByteString -> Int
toIntBS = BS.foldl' step 0
  where
    step acc w = acc * 10 + (fromIntegral w - fromIntegral W8._0)
