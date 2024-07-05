--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen0.ANF
  ( AExp(..)
  , CExp(..)
  , Dec(..)
  , Exp(..)
  , Lam(..)
  , Prim(..)
  , Prog(..)
  , Var(..)
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
newtype Prog = Prog [Dec] deriving (Eq, Ord, Show)

-- | Defines a declaration.
data Dec
  = DecDefine Var Exp
  -- | DecBegin [Dec] -- do we want this?
  | DecExp Exp
    deriving (Eq, Ord, Show)

-- | Defines an expression.
data Exp
  = ExpAtomic AExp
  | ExpComplex CExp
  | ExpLet Var Exp Exp
    deriving (Eq, Ord, Show)

-- | Defines an atomic expression.
data AExp
  = AExpLam Lam
  | AExpVar Var
  | AExpTrue
  | AExpFalse
  | AExpInt Integer
  | AExpPrim Prim [AExp]
    deriving (Eq, Ord, Show)

-- | Defines a complex expression.
data CExp
  = CExpApp [AExp]
  | CExpIf AExp Exp Exp
  | CExpCallCC AExp
  | CExpSet Var AExp
  | CExpLetRec [(Var, AExp)] Exp
    deriving (Eq, Ord, Show)

-- | Defines a variable name.
newtype Var = Var Text deriving (Eq, Ord, Show)

-- | Defines a lambda form.
data Lam = Lam [Var] Exp deriving (Eq, Ord, Show)

-- | Defines a primitive operator.
data Prim
  = PrimAdd
  | PrimSub
  | PrimMul
  | PrimDiv
  | PrimEq
    deriving (Eq, Ord, Show)

-- | Defines a parser.
type Parser = Parsec Void Text

-- | Parses a program.
parseProg :: Parser Prog
parseProg = spaceConsumer >> (Prog <$> some parseDec)

-- | Parses a declaration.
parseDec :: Parser Dec
parseDec = choice
  [ try parseDefine
  , DecExp <$> parseExp
  ]

-- | Parses a definition.
parseDefine :: Parser Dec
parseDefine =
  parens $ do
    void $ symbol "define"
    DecDefine <$> parseVar <*> parseExp

-- | Parses an expression.
parseExp :: Parser Exp
parseExp = choice
  [ try $ ExpAtomic <$> parseAExp
  , try $ ExpComplex <$> parseCExp
  , parseLet
  ]

-- | Parses an atomic expression.
parseAExp :: Parser AExp
parseAExp = choice
  [ try $ AExpTrue <$ symbol "#t"
  , try $ AExpFalse <$ symbol "#f"
  , try $ AExpVar <$> parseVar
  , try $ AExpInt <$> integer
  , try $ AExpLam <$> parseLam
  , parens $ AExpPrim <$> prim <*> some parseAExp
  ]

-- | Parses a complex expression.
parseCExp :: Parser CExp
parseCExp = choice
  [ try parseIf
  , try parseCallCC
  , try parseSet
  , try parseLetrec
  , parseApp
  ]

-- | Parses a variable name.
parseVar :: Parser Var
parseVar = Var <$> identifier

-- | Parses a lambda form.
parseLam :: Parser Lam
parseLam =
  parens $ do
    void $ symbol "λ"
    vars <- parens $ many parseVar
    exp <- parseExp
    pure $ Lam vars exp

-- | Parses an if expression.
parseIf :: Parser CExp
parseIf =
  parens $ do
    void $ symbol "if"
    CExpIf <$> parseAExp <*> parseExp <*> parseExp

-- | Parses a call/cc form.
parseCallCC :: Parser CExp
parseCallCC =
  parens $ do
    void $ symbol "call/cc"
    CExpCallCC <$> parseAExp

-- | Parses a mutation.
parseSet :: Parser CExp
parseSet =
  parens $ do
    void $ symbol "set!"
    CExpSet <$> parseVar <*> parseAExp

-- | Parses an application.
parseApp :: Parser CExp
parseApp = parens $ CExpApp <$> some parseAExp

-- | Parses a binding.
parseBinding :: Parser (Var, AExp)
parseBinding = parens $ (,) <$> parseVar <*> parseAExp

-- | Parses a let expression.
parseLet :: Parser Exp
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
    pure $ ExpLet var exp body

-- | Parses a letrec expression.
parseLetrec :: Parser CExp
parseLetrec =
  parens $ do
    void $ symbol "letrec"
    void $ symbol "("
    bindings <- some parseBinding
    void $ symbol ")"
    body <- parseExp
    pure $ CExpLetRec bindings body

-- | Parses a primary operator.
prim :: Parser Prim
prim = choice
  [ PrimAdd <$ symbol "+"
  , PrimSub <$ symbol "-"
  , PrimMul <$ symbol "*"
  , PrimDiv <$ symbol "/"
  , PrimEq  <$ symbol "="
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
runExpParser :: Text -> Either (ParseErrorBundle Text Void) Exp
runExpParser = runParser parseExp ""

-- | Parses a program.
runProgParser :: Text -> Either (ParseErrorBundle Text Void) Prog
runProgParser = runParser parseProg ""

-- | Parses an expression.
anfParseExp :: Text -> Either Text Exp
anfParseExp = mapLeft renderError . runExpParser

-- | Parses a program.
anfParseProg :: Text -> Either Text Prog
anfParseProg = mapLeft renderError . runProgParser

-- | Tests an expression parse.
testExp :: Text -> IO ()
testExp  = parseTest parseExp

-- | Tests a program parse.
testProg :: Text -> IO ()
testProg  = parseTest parseProg
