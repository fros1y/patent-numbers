module Data.Patent.Number.Parser where
import Prelude
import Data.Patent.Number
import Text.Parsec (ParsecT, char, choice, count, digit, runParser, try, upper, (<?>))
import Text.Parsec.Text (Parser)


type ParseError = Text

parseCountry :: Parser Text
parseCountry = toText <$> count 2 upper

parseSerial :: Parser Text
parseSerial = toText <$> some digit

parseKind :: Parser Text
parseKind =
  "kind code" <??> do
    firstLetter <- upper
    secondLetterOrNumber <- optional $ upper <|> digit
    pure $ toText $ catMaybes [Just firstLetter, secondLetterOrNumber]

(<??>) :: String -> ParsecT s u m a -> ParsecT s u m a
(<??>) = flip (<?>)

epodocParser :: Parser PatentNumber
epodocParser =
  "EPODOC" <??> do
    countryPart <- parseCountry
    serialPart <- parseSerial
    mkPatentNumber' countryPart serialPart <$> parseKind

countrySerialParser :: Parser PatentNumber
countrySerialParser = do
  countryPart <- parseCountry
  serialPart <- parseSerial
  pure $ mkPatentNumber countryPart serialPart Nothing

docdbParser :: Parser PatentNumber
docdbParser =
  "DOCDB" <??> do
    countryPart <- parseCountry
    _ <- char '-'
    serialPart <- parseSerial
    _ <- char '-'
    mkPatentNumber' countryPart serialPart <$> parseKind

patentParser :: Parser PatentNumber
patentParser = choice [try epodocParser, try docdbParser, try countrySerialParser]

parse :: Text -> Either ParseError PatentNumber
parse input = case runParser patentParser () "patentParser" input of
  Left err -> Left $ show err
  Right p -> Right p