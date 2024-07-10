--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Scheme
  ( SchemeBind(..)
  , SchemeDec(..)
  , SchemeExp(..)
  , SchemeProg(..)
  , SchemeRenderOptions(..)
  , SchemeRenderStyle(..)
  , SchemeVar(..)
  , schemeANF
  , schemeParse
  , schemeRender
  , schemeRenderOptions
  ) where

import Prelude hiding (exp)
import Control.Monad ((>=>), forM_, void, when)
import Control.Monad.State (StateT(..), execStateT, get, put, modify)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Gen1.ANF
import Gen1.Parse
import Text.Megaparsec
import Text.Megaparsec.Char

-- <prog> ::= <dec> ...
--
-- <dec> ::= (define (<var> <name> ...) <exp>)
--        |  (define <var> <exp>)
--        |  <exp>
--
-- <exp> ::= (let ([<var> <exp>] ...) <exp>)
--        |  (if <exp> <exp> <exp>)
--        |  (set! <var> <exp>)
--        |  (λ (<name> ...) <exp>)
--        |  (<exp> ...)
--        |  <number>
--        |  <boolean>
--        |  <string>
--        |  <var>

-- | Defines a program.
newtype SchemeProg = SchemeProg [SchemeDec] deriving (Eq, Ord, Show)

-- | Defines a variable name.
newtype SchemeVar = SchemeVar Text deriving (Eq, Ord, Show)

-- | Defines a binding.
data SchemeBind = SchemeBind SchemeVar SchemeExp deriving (Eq, Ord, Show)

-- | Defines a declaration.
data SchemeDec
  = SchemeDecFunc SchemeVar [SchemeVar] SchemeExp
  | SchemeDecDefine SchemeVar SchemeExp
  | SchemeDecBegin [SchemeDec]
  | SchemeDecExp SchemeExp
    deriving (Eq, Ord, Show)

-- | Defines an expression.
data SchemeExp
  = SchemeExpApp [SchemeExp]
  | SchemeExpLet [SchemeBind] SchemeExp
  | SchemeExpLetRec [SchemeBind] SchemeExp
  | SchemeExpIf SchemeExp SchemeExp SchemeExp
  | SchemeExpSet SchemeVar SchemeExp
  | SchemeExpCallCC SchemeExp
  | SchemeExpLam [SchemeVar] SchemeExp
  | SchemeExpVar SchemeVar
  | SchemeExpInt Integer
  | SchemeExpFloat Double
  | SchemeExpBool Bool
  | SchemeExpStr Text
  | SchemeExpChar Char
  | SchemeExpVoid
    deriving (Eq, Ord, Show)

-- | Defines the render style.
data SchemeRenderStyle
  = SchemeRenderNormal
  | SchemeRenderPretty deriving (Eq, Ord, Show)

-- | Defines the render options.
data SchemeRenderOptions = SchemeRenderOptions
  { schemeRenderOptionIndent :: Int
  , schemeRenderOptionStyle  :: SchemeRenderStyle
  } deriving (Eq, Ord, Show)

-- | Defines the render state.
data SchemeRenderState = SchemeRenderState
  { schemeRenderStateColumn  :: Int
  , schemeRenderStateText    :: Text
  , schemeRenderStateOptions :: SchemeRenderOptions
  } deriving (Eq, Ord, Show)

-- | Defines a renderer.
type SchemeRender = StateT SchemeRenderState IO

-- | The default render options.
schemeRenderOptions :: SchemeRenderOptions
schemeRenderOptions = SchemeRenderOptions 2 SchemeRenderNormal

-- | Renders a scheme program.
schemeRender :: SchemeRenderOptions -> SchemeProg -> IO Text
schemeRender opt exp = do
  SchemeRenderState{..} <- execStateT (renderProg exp) $
    SchemeRenderState 0 T.empty opt
  pure $ T.strip schemeRenderStateText

-- | Increases the indentation column.
indentInc :: SchemeRender ()
indentInc = do
  SchemeRenderState{..} <- get
  SchemeRenderOptions{..} <- pure schemeRenderStateOptions
  let c = schemeRenderStateColumn + schemeRenderOptionIndent
  modify $ \s -> s { schemeRenderStateColumn = c }

-- | Decreases the indentation column.
indentDec :: SchemeRender ()
indentDec = do
  SchemeRenderState{..} <- get
  SchemeRenderOptions{..} <- pure schemeRenderStateOptions
  let c = schemeRenderStateColumn - schemeRenderOptionIndent
  modify $ \s -> s { schemeRenderStateColumn = c }

-- | Renders a newline.
renderNewline :: SchemeRender ()
renderNewline = do
  SchemeRenderState{..} <- get
  case schemeRenderOptionStyle schemeRenderStateOptions of
    SchemeRenderNormal ->
      renderSpace
    SchemeRenderPretty ->
      renderText $ "\n" <> T.replicate schemeRenderStateColumn " "

-- | Renders literal text.
renderText :: Text -> SchemeRender ()
renderText x = modify $ \s ->
  s { schemeRenderStateText = T.append (schemeRenderStateText s) x }

-- | Renders an open parenthesis.
renderOpen :: SchemeRender ()
renderOpen = renderText "("

-- | Renders a close parenthesis.
renderClose :: SchemeRender ()
renderClose = renderText ")"

-- | Renders a space character.
renderSpace :: SchemeRender ()
renderSpace = renderText " "

-- | Renders a program.
renderProg :: SchemeProg -> SchemeRender ()
renderProg (SchemeProg decs) = do
  forM_ (zip [0..] decs) $ \(i :: Int, d) -> do
    renderDec d
    renderNewline

-- | Renders a declaration.
renderDec :: SchemeDec -> SchemeRender ()
renderDec = \case
  SchemeDecFunc var vars exp -> do
    renderOpen
    renderText "define"
    renderSpace
    renderOpen
    renderVar var
    forM_ vars $ \v -> do
      renderSpace
      renderVar v
    renderClose
    indentInc
    renderNewline
    renderExp exp
    renderClose
    indentDec
  SchemeDecDefine var exp -> do
    renderOpen
    renderText "define"
    renderSpace
    renderVar var
    renderSpace
    renderExp exp
    renderClose
  SchemeDecBegin decs -> do
    renderOpen
    renderText "begin"
    indentInc
    forM_ decs $ \d -> do
      renderNewline
      renderDec d
    renderClose
    indentDec
  SchemeDecExp exp -> do
    renderExp exp

-- | Renders an expression.
renderExp :: SchemeExp -> SchemeRender ()
renderExp = \case
  SchemeExpApp exps -> do
    renderOpen
    forM_ (zip [0..] exps) $ \(i :: Int, e) -> do
      when (i > 0) renderSpace
      renderExp e
    renderClose
  SchemeExpLet bindings exp -> do
    renderLet "let" bindings exp
  SchemeExpLetRec bindings exp -> do
    renderLet "letrec" bindings exp
  SchemeExpIf exp0 exp1 exp2 -> do
    renderOpen
    renderText "if"
    renderSpace
    renderExp exp0
    indentInc
    renderNewline
    renderExp exp1
    renderNewline
    renderExp exp2
    renderClose
    indentDec
  SchemeExpSet var exp -> do
    renderOpen
    renderText "set!"
    renderSpace
    renderVar var
    renderSpace
    renderExp exp
    renderClose
  SchemeExpCallCC exp -> do
    renderOpen
    renderText "call/cc"
    renderSpace
    renderExp exp
    renderClose
  SchemeExpLam vars exp -> do
    renderOpen
    renderText "λ"
    renderSpace
    renderOpen
    forM_ (zip [0..] vars) $ \(i :: Int, v) -> do
      when (i > 0) renderSpace
      renderVar v
    renderClose
    indentInc
    renderNewline
    renderExp exp
    renderClose
    indentDec
  SchemeExpVar var -> do
    renderVar var
  SchemeExpInt x -> do
    renderText $ T.pack $ show x
  SchemeExpFloat x -> do
    renderText $ T.pack $ show x
  SchemeExpBool True -> do
    renderText "#t"
  SchemeExpBool False -> do
    renderText "#f"
  SchemeExpStr x -> do
    renderText $ "\"" <> x <> "\""
  SchemeExpChar x -> do
    renderText $ T.snoc "#\\" x
  SchemeExpVoid -> do
    renderText "#<void>"

-- | Renders a let style form.
renderLet :: Text -> [SchemeBind] -> SchemeExp -> SchemeRender ()
renderLet name bindings exp = do
  renderOpen
  renderText name
  renderSpace
  renderOpen
  forM_ (zip [0..] bindings) $ \(i :: Int, b) -> do
    when (i > 0) renderSpace
    renderBind b
  renderClose
  indentInc
  renderNewline
  renderExp exp
  renderClose
  indentDec

-- | Renders a binding.
renderBind :: SchemeBind -> SchemeRender ()
renderBind (SchemeBind var exp) = do
  renderOpen
  renderVar var
  renderSpace
  renderExp exp
  renderClose

-- | Renders a variable.
renderVar :: SchemeVar -> SchemeRender ()
renderVar (SchemeVar name) = renderText name

-- | Parses a scheme program.
schemeParse :: Text -> Either Text SchemeProg
schemeParse = mapLeft renderError . runParser parseProg ""

-- | Parses a program.
parseProg :: Parser SchemeProg
parseProg = spaceConsumer >> (SchemeProg <$> many parseDec)

-- | Parses a declaration.
parseDec :: Parser SchemeDec
parseDec = choice
  [ try parseFunc
  , try parseDefine
  , try parseBegin
  , SchemeDecExp <$> parseExp
  ]

-- | Parses a function definition.
parseFunc :: Parser SchemeDec
parseFunc =
  parseParens $ do
    void $ parseSymbol "define"
    void $ parseSymbol "("
    var <- parseVar
    vars <- many parseVar
    void $ parseSymbol ")"
    exp <- parseExp
    pure $ SchemeDecFunc var vars exp

-- | Parses a definition.
parseDefine :: Parser SchemeDec
parseDefine =
  parseParens $ do
    void $ parseSymbol "define"
    SchemeDecDefine <$> parseVar <*> parseExp

-- | Parses a begin.
parseBegin :: Parser SchemeDec
parseBegin =
  parseParens $ do
    void $ parseSymbol "begin"
    SchemeDecBegin <$> many parseDec

-- | Parses an expression.
parseExp :: Parser SchemeExp
parseExp = choice
  [ try parseLam
  , try parseLet
  , try parseLetrec
  , try parseIf
  , try parseSet
  , try parseCallCC
  , try $ SchemeExpVar <$> parseVar
  , try $ SchemeExpInt <$> parseInteger
  , try $ SchemeExpFloat <$> parseDouble
  , try $ SchemeExpBool True <$ parseTrue
  , try $ SchemeExpBool False <$ parseFalse
  , try $ SchemeExpStr <$> parseString
  , try $ SchemeExpChar <$> parseChar
  , try $ SchemeExpVoid <$ parseVoid
  , parseApp
  ]

-- | Parses a variable name.
parseVar :: Parser SchemeVar
parseVar = SchemeVar <$> choice
  [ try $ parseSymbol "+"
  , try $ parseSymbol "-"
  , try $ parseSymbol "*"
  , try $ parseSymbol "/="
  , try $ parseSymbol "/"
  , try $ parseSymbol "="
  , try $ parseSymbol ">="
  , try $ parseSymbol ">"
  , try $ parseSymbol "<="
  , try $ parseSymbol "<"
  , try parseIntrinsic
  , parseIdentifier
  ]

-- | Parses a built-in function.
parseIntrinsic :: Parser Text
parseIntrinsic = do
  a <- parseSymbol "@"
  b <- parseIdentifier
  pure $ a <> b

-- | Parses a lambda form.
parseLam :: Parser SchemeExp
parseLam =
  parseParens $ do
    void $ parseSymbol "λ"
    vars <- parseParens $ many parseVar
    exp <- parseExp
    pure $ SchemeExpLam vars exp

-- | Parses an if expression.
parseIf :: Parser SchemeExp
parseIf =
  parseParens $ do
    void $ parseSymbol "if"
    SchemeExpIf <$> parseExp <*> parseExp <*> parseExp

-- | Parses a call/cc form.
parseCallCC :: Parser SchemeExp
parseCallCC =
  parseParens $ do
    void $ parseSymbol "call/cc"
    SchemeExpCallCC <$> parseExp

-- | Parses a mutation.
parseSet :: Parser SchemeExp
parseSet =
  parseParens $ do
    void $ parseSymbol "set!"
    SchemeExpSet <$> parseVar <*> parseExp

-- | Parses an application.
parseApp :: Parser SchemeExp
parseApp = parseParens $ SchemeExpApp <$> some parseExp

-- | Parses a binding.
parseBinding :: Parser SchemeBind
parseBinding = parseParens $ SchemeBind <$> parseVar <*> parseExp

-- | Parses a let expression.
parseLet :: Parser SchemeExp
parseLet = parseParens $ do
  void $ parseSymbol "let"
  void $ parseSymbol "("
  bindings <- some parseBinding
  void $ parseSymbol ")"
  body <- parseExp
  pure $ SchemeExpLet bindings body

-- | Parses a letrec expression.
parseLetrec :: Parser SchemeExp
parseLetrec = parseParens $ do
  void $ parseSymbol "letrec"
  void $ parseSymbol "("
  bindings <- some parseBinding
  void $ parseSymbol ")"
  body <- parseExp
  pure $ SchemeExpLetRec bindings body

-- | Parses a true literal.
parseTrue :: Parser ()
parseTrue = void $ try (parseSymbol "#t") <|> parseSymbol "#true"

-- | Parses a false literal.
parseFalse :: Parser ()
parseFalse = void $ try (parseSymbol "#f") <|> parseSymbol "#false"

-- | Parses a void literal.
parseVoid :: Parser ()
parseVoid = void $ parseSymbol "#void"

-- | Converts a scheme AST to an ANF AST.
schemeANF :: SchemeProg -> Either Text ANFProg
schemeANF (SchemeProg decs) = do
  ANFProg <$> mapM schemeDecANF decs

-- | Converts a scheme declaration to an ANF declaration.
schemeDecANF :: SchemeDec -> Either Text ANFDec
schemeDecANF = \case
  SchemeDecFunc (SchemeVar var) vars exp ->
    Left $ "scheme not normalized: function " <> var
  SchemeDecDefine var exp ->
    ANFDecDefine <$> schemeVarANF var <*> schemeExpANF exp
  SchemeDecBegin decs ->
    ANFDecBegin <$> mapM schemeDecANF decs
  SchemeDecExp exp ->
    ANFDecExp <$> schemeExpANF exp

-- | Converts a scheme expression to an ANF expression.
schemeExpANF :: SchemeExp -> Either Text ANFExp
schemeExpANF = \case
  SchemeExpApp exps -> do
    aexps <- mapM schemeAtomic exps
    case aexps of
      (ANFAtomicVar var):es | Just p <- schemePrim var -> do
        pure $ ANFExpAtomic $ ANFAtomicPrim p es
      _otherwise -> do
        pure $ ANFExpComplex $ ANFComplexApp aexps
  SchemeExpLet (SchemeBind var exp0:[]) exp1 -> do
    ANFExpLet
      <$> schemeVarANF var
      <*> schemeExpANF exp0
      <*> schemeExpANF exp1
  SchemeExpLet bindings _exp -> do
    Left $ "bad bindings in let: " <> T.intercalate " "
      (map (\(SchemeBind (SchemeVar var) _) -> var) bindings)
  SchemeExpLetRec bindings exp -> do
    e <- ANFComplexLetRec
      <$> mapM schemeBindANF bindings
      <*> schemeExpANF exp
    pure $ ANFExpComplex e
  SchemeExpIf exp0 exp1 exp2 -> do
    e <- ANFComplexIf
      <$> schemeAtomic exp0
      <*> schemeExpANF exp1
      <*> schemeExpANF exp2
    pure $ ANFExpComplex e
  SchemeExpSet var exp -> do
    e <- ANFComplexSet
      <$> schemeVarANF var
      <*> schemeAtomic exp
    pure $ ANFExpComplex e
  SchemeExpCallCC exp -> do
    e <- ANFComplexCallCC <$> schemeAtomic exp
    pure $ ANFExpComplex e
  SchemeExpLam vars exp -> do
    lam <- ANFLam
      <$> mapM schemeVarANF vars
      <*> schemeExpANF exp
    pure $ ANFExpAtomic $ ANFAtomicLam lam
  SchemeExpVar var -> do
    v <- ANFAtomicVar <$> schemeVarANF var
    pure $ ANFExpAtomic v
  SchemeExpInt x -> do
    pure $ ANFExpAtomic $ ANFAtomicInt x
  SchemeExpFloat x -> do
    pure $ ANFExpAtomic $ ANFAtomicFloat x
  SchemeExpBool x -> do
    pure $ ANFExpAtomic $ ANFAtomicBool x
  SchemeExpStr x -> do
    pure $ ANFExpAtomic $ ANFAtomicStr x
  SchemeExpChar x -> do
    pure $ ANFExpAtomic $ ANFAtomicChar x
  SchemeExpVoid -> do
    pure $ ANFExpAtomic $ ANFAtomicVoid

-- | Converts a scheme variable to an ANF variable.
schemeVarANF :: SchemeVar -> Either Text ANFVar
schemeVarANF (SchemeVar var) = pure $ ANFVar var

-- | Converts a scheme binding to an ANF binding.
schemeBindANF :: SchemeBind -> Either Text ANFBind
schemeBindANF (SchemeBind var exp) =
  ANFBind <$> schemeVarANF var <*> schemeAtomic exp

-- | Converts a scheme expression to an ANF atomic.
schemeAtomic :: SchemeExp -> Either Text ANFAtomic
schemeAtomic = schemeExpANF >=> anfAtomic

-- | Converts an ANF expression to an ANF atomic.
anfAtomic :: ANFExp -> Either Text ANFAtomic
anfAtomic = \case
  ANFExpAtomic a -> pure a
  _otherwise -> Left "non-atomic expression"

-- | Converts a scheme variable to an ANF primitive.
schemePrim :: ANFVar -> Maybe ANFPrim
schemePrim (ANFVar var) =
  case var of
    "+"  -> Just ANFPrimAdd
    "-"  -> Just ANFPrimSub
    "*"  -> Just ANFPrimMul
    "/"  -> Just ANFPrimDiv
    "="  -> Just ANFPrimEQ
    "/=" -> Just ANFPrimNE
    ">"  -> Just ANFPrimGT
    ">=" -> Just ANFPrimGE
    "<"  -> Just ANFPrimLT
    "<=" -> Just ANFPrimLE
    _op  -> Nothing
