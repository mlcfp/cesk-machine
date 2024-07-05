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
  , ANFRenderStyle(..)
  , ANFRenderOptions(..)
  , ANFRenderState(..)
  , ANFVar(..)
  , anfParse
  , anfRenderOptions
  , anfRender
  ) where

import Prelude hiding (exp)
import Control.Monad (forM_, void, when)
import Control.Monad.State (StateT(..), execStateT, get, put, modify)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Gen1.Parse
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
--        |  #t  |  #true
--        |  #f  |  #false
--        |  integer
--        |  float
--        |  string
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
-- prim ::=  +  |  -  |  *  |  =

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
  | ANFAtomicPrim ANFPrim [ANFAtomic]
  | ANFAtomicBool Bool
  | ANFAtomicInt Integer
  | ANFAtomicFloat Double
  | ANFAtomicStr Text
  | ANFAtomicVoid
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

-- | Defines a primitive operator.
data ANFPrim
  = ANFPrimAdd
  | ANFPrimSub
  | ANFPrimMul
  | ANFPrimDiv
  | ANFPrimEq
    deriving (Eq, Ord, Show)

-- | Parses an ANF program.
anfParse :: Text -> Either Text ANFProg
anfParse = mapLeft renderError . runParser parseProg ""

-- | Parses a program.
parseProg :: Parser ANFProg
parseProg = spaceConsumer >> (ANFProg <$> some parseDec)

-- | Parses a declaration.
parseDec :: Parser ANFDec
parseDec = choice
  [ try parseDefine
  , try parseBegin
  , ANFDecExp <$> parseExp
  ]

-- | Parses a definition.
parseDefine :: Parser ANFDec
parseDefine =
  parseParens $ do
    void $ parseSymbol "define"
    ANFDecDefine <$> parseVar <*> parseExp

-- | Parses a begin.
parseBegin :: Parser ANFDec
parseBegin =
  parseParens $ do
    void $ parseSymbol "begin"
    ANFDecBegin <$> many parseDec

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
  [ try $ ANFAtomicBool <$> parseBool
  , try $ ANFAtomicVoid <$ parseVoid
  , try $ ANFAtomicVar <$> parseVar
  , try $ ANFAtomicFloat <$> parseDouble
  , try $ ANFAtomicInt <$> parseInteger
  , try $ ANFAtomicStr <$> parseString
  , try $ ANFAtomicLam <$> parseLam
  , parseParens $ ANFAtomicPrim <$> parsePrim <*> some parseAtomic
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
parseVar = ANFVar <$> parseIdentifier

-- | Parses a lambda form.
parseLam :: Parser ANFLam
parseLam =
  parseParens $ do
    void $ parseSymbol "λ"
    vars <- parseParens $ many parseVar
    exp <- parseExp
    pure $ ANFLam vars exp

-- | Parses an if expression.
parseIf :: Parser ANFComplex
parseIf =
  parseParens $ do
    void $ parseSymbol "if"
    ANFComplexIf <$> parseAtomic <*> parseExp <*> parseExp

-- | Parses a call/cc form.
parseCallCC :: Parser ANFComplex
parseCallCC =
  parseParens $ do
    void $ parseSymbol "call/cc"
    ANFComplexCallCC <$> parseAtomic

-- | Parses a mutation.
parseSet :: Parser ANFComplex
parseSet =
  parseParens $ do
    void $ parseSymbol "set!"
    ANFComplexSet <$> parseVar <*> parseAtomic

-- | Parses an application.
parseApp :: Parser ANFComplex
parseApp = parseParens $ ANFComplexApp <$> some parseAtomic

-- | Parses a binding.
parseBinding :: Parser ANFBind
parseBinding = parseParens $ ANFBind <$> parseVar <*> parseAtomic

-- | Parses a let expression.
parseLet :: Parser ANFExp
parseLet =
  parseParens $ do
    void $ parseSymbol "let"
    void $ parseSymbol "("
    void $ parseSymbol "("
    var <- parseVar
    exp <- parseExp
    void $ parseSymbol ")"
    void $ parseSymbol ")"
    body <- parseExp
    pure $ ANFExpLet var exp body

-- | Parses a letrec expression.
parseLetrec :: Parser ANFComplex
parseLetrec =
  parseParens $ do
    void $ parseSymbol "letrec"
    void $ parseSymbol "("
    bindings <- some parseBinding
    void $ parseSymbol ")"
    body <- parseExp
    pure $ ANFComplexLetRec bindings body

-- | Parses a primary operator.
parsePrim :: Parser ANFPrim
parsePrim = choice
  [ ANFPrimAdd <$ parseSymbol "+"
  , ANFPrimSub <$ parseSymbol "-"
  , ANFPrimMul <$ parseSymbol "*"
  , ANFPrimDiv <$ parseSymbol "/"
  , ANFPrimEq  <$ parseSymbol "="
  ]

-- | Parses an expression.
parseBool :: Parser Bool
parseBool = try (True <$ parseTrue) <|> (False <$ parseFalse)

-- | Parses a true literal.
parseTrue :: Parser ()
parseTrue = void $ try (parseSymbol "#true") <|> parseSymbol "#t"

-- | Parses a false literal.
parseFalse :: Parser ()
parseFalse = void $ try (parseSymbol "#false") <|> parseSymbol "#f"

-- | Parses a void literal.
parseVoid :: Parser ()
parseVoid = void $ try (parseSymbol "#void") <|> parseSymbol "#v"

-- | Defines the render style.
data ANFRenderStyle
  = ANFRenderNormal
  | ANFRenderPretty deriving (Eq, Ord, Show)

-- | Defines the render options.
data ANFRenderOptions = ANFRenderOptions
  { anfRenderOptionIndent :: Int
  , anfRenderOptionStyle  :: ANFRenderStyle
  } deriving (Eq, Ord, Show)

-- | Defines the render state.
data ANFRenderState = ANFRenderState
  { anfRenderStateColumn  :: Int
  , anfRenderStateText    :: Text
  , anfRenderStateOptions :: ANFRenderOptions
  } deriving (Eq, Ord, Show)

-- | Defines a renderer.
type ANFRender = StateT ANFRenderState IO

-- | The default render options.
anfRenderOptions :: ANFRenderOptions
anfRenderOptions = ANFRenderOptions 2 ANFRenderNormal

-- | Renders an ANF program.
anfRender :: ANFRenderOptions -> ANFProg -> IO Text
anfRender opt exp = do
  ANFRenderState{..} <- execStateT (anfRenderProg exp) $
    ANFRenderState 0 T.empty opt
  pure $ T.strip anfRenderStateText

-- | Renders a program.
anfRenderProg :: ANFProg -> ANFRender ()
anfRenderProg (ANFProg decs) = do
  forM_ (zip [0..] decs) $ \(i :: Int, d) -> do
    anfRenderDec d
    renderNewline

-- | Renders a declaration.
anfRenderDec :: ANFDec -> ANFRender ()
anfRenderDec = \case
  ANFDecDefine var exp -> do
    renderOpen
    renderText "define"
    renderSpace
    anfRenderVar var
    renderSpace
    anfRenderExp exp
    renderClose
  ANFDecBegin decs -> do
    renderOpen
    renderText "begin"
    indentInc
    forM_ decs $ \d -> do
      renderNewline
      anfRenderDec d
    renderClose
    indentDec
  ANFDecExp exp -> do
    anfRenderExp exp

-- | Renders an expression.
anfRenderExp :: ANFExp -> ANFRender ()
anfRenderExp = \case
  ANFExpAtomic x -> do
    anfRenderAtomic x
  ANFExpComplex x -> do
    anfRenderComplex x
  ANFExpLet v e0 e1 -> do
    anfRenderLet v e0 e1

-- | Renders an expression.
anfRenderAtomic :: ANFAtomic -> ANFRender ()
anfRenderAtomic = \case
  ANFAtomicLam (ANFLam vars exp) -> do
    renderOpen
    renderText "λ"
    renderSpace
    renderOpen
    forM_ (zip [0..] vars) $ \(i :: Int, v) -> do
      when (i > 0) renderSpace
      anfRenderVar v
    renderClose
    indentInc
    renderNewline
    anfRenderExp exp
    renderClose
    indentDec
  ANFAtomicVar var -> do
    anfRenderVar var
  ANFAtomicInt x -> do
    renderText $ T.pack $ show x
  ANFAtomicFloat x -> do
    renderText $ T.pack $ show x
  ANFAtomicBool True -> do
    renderText "#t"
  ANFAtomicBool False -> do
    renderText "#f"
  ANFAtomicVoid -> do
    renderText "#void"
  ANFAtomicStr x -> do
    renderText "\""
    renderText x
    renderText "\""
  ANFAtomicPrim prim aexps -> do
    renderOpen
    anfRenderPrim prim
    renderSpace
    forM_ (zip [0..] aexps) $ \(i :: Int, e) -> do
      when (i > 0) renderSpace
      anfRenderAtomic e
    renderClose

-- | Renders an expression.
anfRenderComplex :: ANFComplex -> ANFRender ()
anfRenderComplex = \case
  ANFComplexApp exps -> do
    renderOpen
    forM_ (zip [0..] exps) $ \(i :: Int, e) -> do
      when (i > 0) renderSpace
      anfRenderAtomic e
    renderClose
  ANFComplexIf aexp exp0 exp1 -> do
    renderOpen
    renderText "if"
    renderSpace
    anfRenderAtomic aexp
    indentInc
    renderNewline
    anfRenderExp exp0
    renderNewline
    anfRenderExp exp1
    renderClose
    indentDec
  ANFComplexCallCC aexp -> do
    renderOpen
    renderText "call/cc"
    renderSpace
    anfRenderAtomic aexp
    renderClose
  ANFComplexSet var aexp -> do
    renderOpen
    renderText "set!"
    renderSpace
    anfRenderVar var
    renderSpace
    anfRenderAtomic aexp
    renderClose
  ANFComplexLetRec bindings exp -> do
    renderOpen
    renderText "letrec"
    renderSpace
    renderOpen
    forM_ (zip [0..] bindings) $ \(i :: Int, b) -> do
      when (i > 0) renderSpace
      anfRenderBind b
    renderClose
    indentInc
    renderNewline
    anfRenderExp exp
    renderClose
    indentDec

-- | Renders a let style form.
anfRenderLet :: ANFVar -> ANFExp -> ANFExp -> ANFRender ()
anfRenderLet var exp0 exp1 = do
  renderOpen
  renderText "let"
  renderSpace
  renderOpen
  renderOpen
  anfRenderVar var
  renderSpace
  anfRenderExp exp0
  renderClose
  renderClose
  indentInc
  renderNewline
  anfRenderExp exp1
  renderClose
  indentDec

-- | Renders a binding.
anfRenderBind :: ANFBind -> ANFRender ()
anfRenderBind (ANFBind var aexp) = do
  renderOpen
  anfRenderVar var
  renderSpace
  anfRenderAtomic aexp
  renderClose

-- | Renders a variable.
anfRenderVar :: ANFVar -> ANFRender ()
anfRenderVar (ANFVar name) = renderText name

-- | Renders a primitive operation.
anfRenderPrim :: ANFPrim -> ANFRender ()
anfRenderPrim = renderText . \case
  ANFPrimAdd -> "+"
  ANFPrimSub -> "-"
  ANFPrimMul -> "*"
  ANFPrimDiv -> "/"
  ANFPrimEq  -> "="

-- | Increases the indentation column.
indentInc :: ANFRender ()
indentInc = do
  ANFRenderState{..} <- get
  ANFRenderOptions{..} <- pure anfRenderStateOptions
  anfRenderStateColumn <- pure $ anfRenderStateColumn + anfRenderOptionIndent
  put ANFRenderState{..}

-- | Decreases the indentation column.
indentDec :: ANFRender ()
indentDec = do
  ANFRenderState{..} <- get
  ANFRenderOptions{..} <- pure anfRenderStateOptions
  anfRenderStateColumn <- pure $ anfRenderStateColumn - anfRenderOptionIndent
  put ANFRenderState{..}

-- | Renders a newline.
renderNewline :: ANFRender ()
renderNewline = do
  ANFRenderState{..} <- get
  case anfRenderOptionStyle anfRenderStateOptions of
    ANFRenderNormal ->
      renderSpace
    ANFRenderPretty ->
      renderText $ "\n" <> T.replicate anfRenderStateColumn " "

-- | Renders literal text.
renderText :: Text -> ANFRender ()
renderText x = modify $ \s ->
  s { anfRenderStateText = T.append (anfRenderStateText s) x }

-- | Renders an open parenthesis.
renderOpen :: ANFRender ()
renderOpen = renderText "("

-- | Renders a close parenthesis.
renderClose :: ANFRender ()
renderClose = renderText ")"

-- | Renders a space character.
renderSpace :: ANFRender ()
renderSpace = renderText " "
