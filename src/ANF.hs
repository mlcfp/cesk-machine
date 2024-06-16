--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module ANF
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

import Control.Monad (void)
import Data.Either.Combinators (mapLeft)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--
-- <prog> ::= <dec> ...
--
-- <dec> ::=  (define <var> <exp>)
--         |  (begin <dec> ...)
--         |  <exp>
--
-- lam ::= (λ (var1 ... varN) exp)
--
-- aexp ::=  lam
--        |  var
--        |  #t  |  #f
--        |  integer
--        |  (prim aexp1 ... aexpN)
--
-- cexp ::=  (aexp0 aexp1 ... aexpN)
--        |  (if aexp exp exp)
--        |  (call/cc aexp)
--        |  (set! var aexp)
--        |  (letrec ((var1 aexp1) ... (varN aexpN)) exp)
--
-- exp ::=  aexp
--       |  cexp
--       |  (let ((var exp)) exp)
--
--  prim ::=  +  |  -  |  *  |  =


newtype Prog = Prog [Dec] deriving (Eq, Ord, Show)

data Dec
  = DecDefine Var Exp -- in ANF the exp can only be a Lam
  -- | DecBegin [Dec] -- do we want this?
  | DecExp Exp
  deriving (Eq, Ord, Show)

data Exp
  = ExpAtomic AExp
  | ExpComplex CExp
  | ExpLet Var Exp Exp
  deriving (Eq, Ord, Show)

data AExp
  = AExpLam Lam
  | AExpVar Var
  | AExpTrue
  | AExpFalse
  | AExpInt Integer
  | AExpPrim Prim [AExp]
  deriving (Eq, Ord, Show)

data CExp
  = CExpApp [AExp]
  | CExpIf AExp Exp Exp
  | CExpCallCC AExp
  | CExpSet Var AExp
  | CExpLetRec [(Var, AExp)] Exp
  deriving (Eq, Ord, Show)

newtype Var = Var Text deriving (Eq, Ord, Show)

data Lam = Lam [Var] Exp deriving (Eq, Ord, Show)

data Prim
  = PrimAdd
  | PrimSub
  | PrimMul
  | PrimDiv
  | PrimEq
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

parseProg :: Parser Prog
parseProg = spaceConsumer >> (Prog <$> some parseDec)

parseDec :: Parser Dec
parseDec = choice
  [ try parseDefine
  , DecExp <$> parseExp
  ]

parseDefine :: Parser Dec
parseDefine =
  parens $ do
    void $ symbol "define"
    DecDefine <$> parseVar <*> parseExp

parseExp :: Parser Exp
parseExp = choice
  [ try $ ExpAtomic <$> parseAExp
  , try $ ExpComplex <$> parseCExp
  , parseLet
  ]

parseAExp :: Parser AExp
parseAExp = choice
  [ try $ AExpTrue <$ symbol "#t"
  , try $ AExpFalse <$ symbol "#f"
  , try $ AExpVar <$> parseVar
  , try $ AExpInt <$> integer
  , try $ AExpLam <$> parseLam
  , parens $ AExpPrim <$> prim <*> some parseAExp
  ]

parseCExp :: Parser CExp
parseCExp = choice
  [ try parseIf
  , try parseCallCC
  , try parseSet
  , try parseLetrec
  , parseApp
  ]

parseVar :: Parser Var
parseVar = Var <$> identifier

parseLam :: Parser Lam
parseLam =
  parens $ do
    void $ symbol "λ"
    vars <- parens $ many parseVar
    exp <- parseExp
    pure $ Lam vars exp

parseIf :: Parser CExp
parseIf =
  parens $ do
    void $ symbol "if"
    CExpIf <$> parseAExp <*> parseExp <*> parseExp

parseCallCC :: Parser CExp
parseCallCC =
  parens $ do
    void $ symbol "call/cc"
    CExpCallCC <$> parseAExp

parseSet :: Parser CExp
parseSet =
  parens $ do
    void $ symbol "set!"
    CExpSet <$> parseVar <*> parseAExp

parseApp :: Parser CExp
parseApp = parens $ CExpApp <$> some parseAExp

parseBinding :: Parser (Var, AExp)
parseBinding = parens $ (,) <$> parseVar <*> parseAExp

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

parseLetrec :: Parser CExp
parseLetrec =
  parens $ do
    void $ symbol "letrec"
    void $ symbol "("
    bindings <- some parseBinding
    void $ symbol ")"
    body <- parseExp
    pure $ CExpLetRec bindings body

prim :: Parser Prim
prim = choice
  [ PrimAdd <$ symbol "+"
  , PrimSub <$ symbol "-"
  , PrimMul <$ symbol "*"
  , PrimDiv <$ symbol "/"
  , PrimEq  <$ symbol "="
  ]

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1
    (L.skipLineComment ";")
    (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

integer :: Parser Integer
integer = lexeme L.decimal

identifier :: Parser Text
identifier = lexeme $ do
  x <- some letterChar
  y <- many alphaNumChar
  pure $ T.pack x <> T.pack y

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- stringLiteral :: Parser String
-- stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- float :: Parser Double
-- float = lexeme L.float


runExpParser :: Text -> Either (ParseErrorBundle Text Void) Exp
runExpParser = runParser parseExp ""

runProgParser :: Text -> Either (ParseErrorBundle Text Void) Prog
runProgParser = runParser parseProg ""

renderError :: ParseErrorBundle Text Void -> Text
renderError = T.pack . errorBundlePretty

anfParseExp :: Text -> Either Text Exp
anfParseExp = mapLeft renderError . runExpParser

anfParseProg :: Text -> Either Text Prog
anfParseProg = mapLeft renderError . runProgParser

testExp :: Text -> IO ()
testExp  = parseTest parseExp

testProg :: Text -> IO ()
testProg  = parseTest parseProg
