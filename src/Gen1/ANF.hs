--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.ANF
  ( ANFAtomic(..)
  , ANFBind(..)
  , ANFComplex(..)
  , ANFDec(..)
  , ANFExp(..)
  , ANFLam(..)
  , ANFPrim(..)
  , ANFProg(..)
  , ANFVar(..)
  , anfParseExp
  , anfParseProg
  ) where

import Prelude hiding (exp)
import Control.Monad (void)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Defines a program.
newtype ANFProg = ANFProg [ANFDec] deriving (Eq, Ord, Show)

-- | Defines a declaration.
data ANFDec
  = ANFDecDefine ANFVar ANFExp
  | ANFDecBegin [ANFDec]
  | ANFDecExp ANFExp
    deriving (Eq, Ord, Show)

-- | Defines an expression.
data ANFExp
  = ANFExpAtomic ANFAtomic
  | ANFExpComplex ANFComplex
  | ANFExpLet ANFVar ANFExp ANFExp
    deriving (Eq, Ord, Show)

-- | Defines an atomic expression.
data ANFAtomic
  = ANFAtomicLam ANFLam
  | ANFAtomicVar ANFVar
  | ANFAtomicTrue
  | ANFAtomicFalse
  | ANFAtomicInt Integer
  | ANFAtomicPrim ANFPrim [ANFAtomic]
    deriving (Eq, Ord, Show)

-- | Defines a complex expression.
data ANFComplex
  = ANFComplexApp [ANFAtomic]
  | ANFComplexIf ANFAtomic ANFExp ANFExp
  | ANFComplexCallCC ANFAtomic
  | ANFComplexSet ANFVar ANFAtomic
  | ANFComplexLetRec [ANFBind] ANFExp
    deriving (Eq, Ord, Show)

-- | Defines a variable name.
newtype ANFVar = ANFVar Text deriving (Eq, Ord, Show)

-- | Defines a binding.
data ANFBind = ANFBind ANFVar ANFAtomic deriving (Eq, Ord, Show)

-- | Defines a lambda form.
data ANFLam = ANFLam [ANFVar] ANFExp deriving (Eq, Ord, Show)

-- | Defines a primative operator.
data ANFPrim
  = ANFPrimAdd
  | ANFPrimSub
  | ANFPrimMul
  | ANFPrimDiv
  | ANFPrimEq
    deriving (Eq, Ord, Show)

-- | Defines a parser.
type Parser = Parsec Void Text

-- | Parses a program.
parseProg :: Parser ANFProg
parseProg = spaceConsumer >> (ANFProg <$> some parseDec)

-- | Parses a declaration.
parseDec :: Parser ANFDec
parseDec = choice
  [ try parseDefine
  , ANFDecExp <$> parseExp
  ]

-- | Parses a definition.
parseDefine :: Parser ANFDec
parseDefine =
  parens $ do
    void $ symbol "define"
    ANFDecDefine <$> parseVar <*> parseExp

-- | Parses an expression.
parseExp :: Parser ANFExp
parseExp = choice
  [ try $ ANFExpAtomic <$> parseAtomic
  , try $ ANFExpComplex <$> parseComplex
  , parseLet
  ]

-- | Parses an atomic expression.
parseAtomic :: Parser ANFAtomic
parseAtomic = choice
  [ try $ ANFAtomicTrue <$ symbol "#t"
  , try $ ANFAtomicFalse <$ symbol "#f"
  , try $ ANFAtomicVar <$> parseVar
  , try $ ANFAtomicInt <$> integer
  , try $ ANFAtomicLam <$> parseLam
  , parens $ ANFAtomicPrim <$> parsePrim <*> some parseAtomic
  ]

-- | Parses a complex expression.
parseComplex :: Parser ANFComplex
parseComplex = choice
  [ try parseIf
  , try parseCallCC
  , try parseSet
  , try parseLetrec
  , parseApp
  ]

-- | Parses a variable name.
parseVar :: Parser ANFVar
parseVar = ANFVar <$> identifier

-- | Parses a lambda form.
parseLam :: Parser ANFLam
parseLam =
  parens $ do
    void $ symbol "λ"
    vars <- parens $ many parseVar
    exp <- parseExp
    pure $ ANFLam vars exp

-- | Parses an if expression.
parseIf :: Parser ANFComplex
parseIf =
  parens $ do
    void $ symbol "if"
    ANFComplexIf <$> parseAtomic <*> parseExp <*> parseExp

-- | Parses a call/cc form.
parseCallCC :: Parser ANFComplex
parseCallCC =
  parens $ do
    void $ symbol "call/cc"
    ANFComplexCallCC <$> parseAtomic

-- | Parses a mutation.
parseSet :: Parser ANFComplex
parseSet =
  parens $ do
    void $ symbol "set!"
    ANFComplexSet <$> parseVar <*> parseAtomic

-- | Parses an application.
parseApp :: Parser ANFComplex
parseApp = parens $ ANFComplexApp <$> some parseAtomic

-- | Parses a binding.
parseBinding :: Parser ANFBind
parseBinding = parens $ ANFBind <$> parseVar <*> parseAtomic

-- | Parses a let expression.
parseLet :: Parser ANFExp
parseLet =
  parens $ do
    void $ symbol "let"
    void $ symbol "("
    void $ symbol "("
    var <- parseVar
    exp <- parseExp
    void $ symbol ")"
    void $ symbol ")"
    body <- parseExp
    pure $ ANFExpLet var exp body

-- | Parses a letrec expression.
parseLetrec :: Parser ANFComplex
parseLetrec =
  parens $ do
    void $ symbol "letrec"
    void $ symbol "("
    bindings <- some parseBinding
    void $ symbol ")"
    body <- parseExp
    pure $ ANFComplexLetRec bindings body

-- | Parses a primary operator.
parsePrim :: Parser ANFPrim
parsePrim = choice
  [ ANFPrimAdd <$ symbol "+"
  , ANFPrimSub <$ symbol "-"
  , ANFPrimMul <$ symbol "*"
  , ANFPrimDiv <$ symbol "/"
  , ANFPrimEq  <$ symbol "="
  ]

-- | Parses whitespace and comments.
spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

-- | Parses a lexeme.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parses a symbol.
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Parsers an integer.
integer :: Parser Integer
integer = lexeme L.decimal

-- | Parses a floating point number.
float :: Parser Double
float = lexeme L.float

-- | Paeses a string.
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- | Parses an identifier.
identifier :: Parser Text
identifier = lexeme $ do
  x <- some letterChar
  y <- many alphaNumChar
  pure $ T.pack x <> T.pack y

-- | Parses something between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Renders an error.
renderError :: ParseErrorBundle Text Void -> Text
renderError = T.pack . errorBundlePretty

-- | Parses an expression.
runExpParser :: Text -> Either (ParseErrorBundle Text Void) ANFExp
runExpParser = runParser parseExp ""

-- | Parses a program.
runProgParser :: Text -> Either (ParseErrorBundle Text Void) ANFProg
runProgParser = runParser parseProg ""

-- | Parses an expression.
anfParseExp :: Text -> Either Text ANFExp
anfParseExp = mapLeft renderError . runExpParser

-- | Parses a program.
anfParseProg :: Text -> Either Text ANFProg
anfParseProg = mapLeft renderError . runProgParser

-- | Tests an expression parse.
testExp :: Text -> IO ()
testExp  = parseTest parseExp

-- | Tests a program parse.
testProg :: Text -> IO ()
testProg  = parseTest parseProg
