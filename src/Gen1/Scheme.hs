--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Scheme
  ( RenderOptions(..)
  , RenderStyle(..)
  , SchemeBind(..)
  , SchemeDec(..)
  , SchemeExp(..)
  , SchemeProg(..)
  , SchemeVar(..)
  , renderOptions
  , schemeParse
  , schemeRender
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
  | SchemeExpVoid
    deriving (Eq, Ord, Show)

-- | Defines the render style.
data RenderStyle = RenderNormal | RenderPretty deriving (Eq, Ord, Show)

-- | Defines the render options.
data RenderOptions = RenderOptions
  { renderOptionIndent :: Int
  , renderOptionStyle  :: RenderStyle
  } deriving (Eq, Ord, Show)

-- | Defines the render state.
data RenderState = RenderState
  { renderStateColumn  :: Int
  , renderStateText    :: Text
  , renderStateOptions :: RenderOptions
  } deriving (Eq, Ord, Show)

-- | Defines a renderer.
type Render = StateT RenderState IO

-- | The default render options.
renderOptions :: RenderOptions
renderOptions = RenderOptions 2 RenderNormal

-- | Renders a scheme program.
schemeRender :: RenderOptions -> SchemeProg -> IO Text
schemeRender opt exp = do
  RenderState{..} <- execStateT (renderProg exp) $
    RenderState 0 T.empty opt
  pure $ T.strip renderStateText

-- | Increases the indentation column.
indentInc :: Render ()
indentInc = do
  RenderState{..} <- get
  RenderOptions{..} <- pure renderStateOptions
  renderStateColumn <- pure $ renderStateColumn + renderOptionIndent
  put RenderState{..}

-- | Decreases the indentation column.
indentDec :: Render ()
indentDec = do
  RenderState{..} <- get
  RenderOptions{..} <- pure renderStateOptions
  renderStateColumn <- pure $ renderStateColumn - renderOptionIndent
  put RenderState{..}

-- | Renders a newline.
renderNewline :: Render ()
renderNewline = do
  RenderState{..} <- get
  case renderOptionStyle renderStateOptions of
    RenderNormal ->
      renderSpace
    RenderPretty ->
      renderText $ "\n" <> T.replicate renderStateColumn " "

-- | Renders literal text.
renderText :: Text -> Render ()
renderText x = modify $ \s ->
  s { renderStateText = T.append (renderStateText s) x }

-- | Renders an open parenthesis.
renderOpen :: Render ()
renderOpen = renderText "("

-- | Renders a close parenthesis.
renderClose :: Render ()
renderClose = renderText ")"

-- | Renders a space character.
renderSpace :: Render ()
renderSpace = renderText " "

-- | Renders a program.
renderProg :: SchemeProg -> Render ()
renderProg (SchemeProg decs) = do
  forM_ (zip [0..] decs) $ \(i :: Int, d) -> do
    renderDec d
    renderNewline

-- | Renders a declaration.
renderDec :: SchemeDec -> Render ()
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
renderExp :: SchemeExp -> Render ()
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
    renderText "\""
    renderText x
    renderText "\""
  SchemeExpVoid -> do
    renderText "#<void>"

-- | Renders a let style form.
renderLet :: Text -> [SchemeBind] -> SchemeExp -> Render ()
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
renderBind :: SchemeBind -> Render ()
renderBind (SchemeBind var exp) = do
  renderOpen
  renderVar var
  renderSpace
  renderExp exp
  renderClose

-- | Renders a variable.
renderVar :: SchemeVar -> Render ()
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
  , try $ SchemeExpVoid <$ parseVoid
  , parseApp
  ]

-- | Parses a variable name.
parseVar :: Parser SchemeVar
parseVar = SchemeVar <$> choice
  [ try $ parseSymbol "+"
  , try $ parseSymbol "-"
  , try $ parseSymbol "*"
  , try $ parseSymbol "/"
  , try $ parseSymbol "="
  , parseIdentifier
  ]

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
