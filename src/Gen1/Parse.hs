--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Parse
  ( Parser
  , parseDouble
  , parseIdentifier
  , parseInteger
  , parseLexeme
  , parseParens
  , spaceConsumer
  , parseString
  , parseSymbol
  , renderError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Defines a parser.
type Parser = Parsec Void Text

-- | Parses whitespace and comments.
spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

-- | Parses a lexeme.
parseLexeme :: Parser a -> Parser a
parseLexeme = L.lexeme spaceConsumer

-- | Parses a symbol.
parseSymbol :: Text -> Parser Text
parseSymbol = L.symbol spaceConsumer

-- | Parsers an integer.
parseInteger :: Parser Integer
parseInteger = parseLexeme L.decimal

-- | Parses a floating point number.
parseDouble :: Parser Double
parseDouble = parseLexeme L.float

-- | Paeses a string.
parseString :: Parser Text
parseString = fmap T.pack $ char '\"' *> manyTill L.charLiteral (char '\"')

-- | Parses an identifier.
parseIdentifier :: Parser Text
parseIdentifier = parseLexeme $ do
  x <- some letterChar
  y <- many alphaNumChar
  pure $ T.pack x <> T.pack y

-- | Parses something between parentheses.
parseParens :: Parser a -> Parser a
parseParens = between (parseSymbol "(") (parseSymbol ")")

-- | Renders an error.
renderError :: ParseErrorBundle Text Void -> Text
renderError = T.pack . errorBundlePretty
