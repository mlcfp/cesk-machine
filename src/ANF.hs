--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module ANF
  ( AExp(..)
  , CExp(..)
  , Exp(..)
  , Lam(..)
  , Prim(..)
  , Prog(..)
  , Var(..)
  , parseANF
  , runTest
  ) where

import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- lam ::= (λ (var1 ... varN) exp)

--  aexp ::= lam
--        |  var
--        |  #t  |  #f
--        |  integer
--        |  (prim aexp1 ... aexpN)

--  cexp ::= (aexp0 aexp1 ... aexpN)
--        |  (if aexp exp exp)
--        |  (call/cc aexp)
--        |  (set! var aexp)
--        |  (letrec ((var1 aexp1) ... (varN aexpN)) exp)

--  exp ::= aexp
--       |  cexp
--       |  (let ((var exp)) exp)

--  prim ::= +  |  -  |  *  |  =


newtype Var = Var Text deriving (Eq, Ord, Show)

data Lam = Lam [Var] Exp deriving (Eq, Ord, Show)

-- data Binding = Binding Var AExp deriving (Eq, Ord, Show)

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

data Exp
  = ExpAtomic AExp
  | ExpComplex CExp
  | ExpLet Var Exp Exp
  deriving (Eq, Ord, Show)

data Prim
  = PrimAdd
  | PrimSub
  | PrimMul
  | PrimDiv
  | PrimEq
  deriving (Eq, Ord, Show)

data Prog = Prog Exp deriving (Eq, Ord, Show)


type Parser = Parsec Void Text

parseVar :: Parser Var
parseVar = Var <$> identifier

parseLam :: Parser Lam
parseLam =
  parens $ do
    void $ symbol "λ"
    vars <- parens $ many parseVar
    exp <- parseExp
    pure $ Lam vars exp

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

parseExp :: Parser Exp
parseExp = choice
  [ try $ ExpAtomic <$> parseAExp
  , try $ ExpComplex <$> parseCExp
  , parseLet
  ]

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
parseApp =
  parens $ do
    CExpApp <$> some parseAExp

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

sc :: Parser ()
-- sc = L.space space1 empty empty
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
-- integer :: Parser Int
integer = lexeme L.decimal

identifier :: Parser Text
identifier = lexeme $ do
  x <- some letterChar
  y <- many alphaNumChar
  pure $ T.pack x <> T.pack y

-- parens :: Applicative m => m open -> m close -> m a -> m a
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- stringLiteral :: Parser String
-- stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- integer :: Parser Integer
-- integer = lexeme L.decimal

-- float :: Parser Double
-- float = lexeme L.float


parseANF :: Text -> Either (ParseErrorBundle Text Void) Exp
parseANF = runParser parseExp ""

-- parseANF :: Text -> Either (ParseErrorBundle Text Void) Var
-- parseANF :: Text -> Either (ParseErrorBundle Text Void) (Var, Int, Var)
-- parseANF :: Text -> Either (ParseErrorBundle Text Void) (Var, Int)
-- parseANF :: Text -> Either (ParseErrorBundle Text Void) ()
-- parseANF = runParser parseANFTest ""

runTest :: Text -> IO ()
runTest  = parseTest parseExp
