--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Scheme
  ( SchemeBind(..)
  , SchemeDec(..)
  , SchemeExp(..)
  , SchemePrim(..)
  , SchemeProg(..)
  , SchemeVar(..)
  -- , schemeParseExp
  -- , schemeParseProg
  , schemeRender
  ) where

-- Input language:

-- <prog> ::= <dec> ...

-- <dec> ::= (define (<var> <name> ...) <exp>)
--        |  (define <var> <exp>)
--        |  <exp>

-- <exp> ::= (let ([<var> <exp>] ...) <exp>)
--        |  (if <exp> <exp> <exp>)
--        |  (set! <var> <exp>)
--        |  (λ (<name> ...) <exp>)
--        |  (<prim> exp1 ... expN)
--        |  <number>
--        |  <boolean>
--        |  <string>
--        |  <var>
--
-- <prim> ::=  +  |  -  |  *  |  =


-- Output language:

-- <prog> ::= <dec> ...

-- <dec> ::= (define <var> <exp>)
--        |  (begin <dec> ...)
--        |  <exp>

-- <aexp> ::= (λ (<name> ...) <exp>)
--         |  <number>
--         |  <boolean>
--         |  <string>
--         |  <var>
--         |  (void)

-- <cexp> ::= (<aexp> <aexp> ...)
--         |  (if <aexp> <exp> <exp>)
--         |  (set! <var> <exp>)

-- <exp> ::= (let ([<var> <cexp>]) <exp>)
--        |  <aexp>
--        |  <cexp>



import Prelude hiding (exp)
import Control.Monad (forM_, void)
import Control.Monad.State (StateT(..), execStateT, get, put, modify)
import Data.Either.Combinators (mapLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L



-- <prog> ::= <dec> ...

-- <dec> ::= (define (<var> <name> ...) <exp>)
--        |  (define <var> <exp>)
--        |  <exp>

-- <exp> ::= (let ([<var> <exp>] ...) <exp>)
--        |  (if <exp> <exp> <exp>)
--        |  (set! <var> <exp>)
--        |  (λ (<name> ...) <exp>)
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
  = SchemeApp [SchemeExp]
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
  | SchemeExpPrim SchemePrim [SchemeExp]
  | SchemeExpVoid
    deriving (Eq, Ord, Show)

-- | Defines a primative operator.
data SchemePrim
  = SchemePrimAdd
  | SchemePrimSub
  | SchemePrimMul
  | SchemePrimDiv
  | SchemePrimEq
    deriving (Eq, Ord, Show)

data RenderStyle = RenderNormal | RenderPretty deriving (Eq, Ord, Show)

-- data RenderState = RenderState
--   { stateLevel :: Int
--   , stateStyle :: RenderStyle
--   } deriving (Eq, Ord, Show)

-- type Render = StateT RenderState IO

-- schemeRender :: SchemeExp -> IO Text
-- schemeRender exp =
--   evalStateT (schemeRenderExp exp) $ RenderState 0 RenderNormal

-- schemeRenderProg :: SchemeProg -> Render Text
-- schemeRenderProg (SchemeProg decs) = do
--   d <- forM decs schemeRenderDec
--   pure $ T.intercalate " " d

-- schemeRenderDec :: SchemeDec -> Render Text
-- schemeRenderDec = \case
--   SchemeDecFunc var vars exp -> do
--     undefined
--   SchemeDecDefine var exp -> do
--     undefined
--   SchemeDecBegin decs -> do
--     undefined
--   SchemeDecExp exp -> do
--     undefined

-- schemeRenderExp :: SchemeExp -> Render Text
-- schemeRenderExp = \case
--   SchemeApp exps -> do
--     forM exps schemeRenderExp >>= schemeRenderParen
--   SchemeExpLet bindings exp -> do
--     bind <- forM bindings schemeRenderBind >>= schemeRenderParen
--     body <- schemeRenderExp exp >>= \x -> schemeRenderParen [x]
--     schemeRenderParen ["let", bind, body]
--   SchemeExpLetRec bindings exp -> do
--     bind <- forM bindings schemeRenderBind >>= schemeRenderParen
--     body <- schemeRenderExp exp >>= \x -> schemeRenderParen [x]
--     schemeRenderParen ["letrec", bind, body]
--   SchemeExpIf exp0 exp1 exp2 -> do
--     e0 <- schemeRenderExp exp0
--     e1 <- schemeRenderExp exp1
--     e2 <- schemeRenderExp exp2
--     schemeRenderParen ["if", e0, e1, e2]
--   SchemeExpSet (SchemeVar var) exp -> do
--     e <- schemeRenderExp exp
--     schemeRenderParen ["set!", var, e]
--   SchemeExpCallCC exp -> do
--     e <- schemeRenderExp exp
--     schemeRenderParen ["call/cc", e]
--   SchemeExpLam vars exp -> do
--     args <- forM vars schemeRenderVar >>= schemeRenderParen
--     body <- schemeRenderExp exp
--     schemeRenderParen ["λ", args, body]
--   SchemeExpVar var -> do
--     schemeRenderVar var
--   SchemeExpInt x -> do
--     pure $ T.pack $ show x
--   SchemeExpFloat x -> do
--     pure $ T.pack $ show x
--   SchemeExpBool True -> do
--     pure "#t"
--   SchemeExpBool False -> do
--     pure "#f"
--   SchemeExpStr x -> do
--     pure $ "\"" <> x <> "\""
--   SchemeExpPrim prim exps -> do
--     p <- schemeRenderPrim prim
--     es <- forM exps schemeRenderExp
--     schemeRenderParen $ p : es
--   SchemeExpVoid -> do
--     pure "void"

-- schemeRenderBind :: SchemeBind -> Render Text
-- schemeRenderBind (SchemeBind var exp) = do
--   v <- schemeRenderVar var
--   e <- schemeRenderExp exp
--   schemeRenderParen [v, e]

-- schemeRenderVar :: SchemeVar -> Render Text
-- schemeRenderVar (SchemeVar name) = pure name

-- schemeRenderPrim :: SchemePrim -> Render Text
-- schemeRenderPrim = pure . \case
--   SchemePrimAdd -> "+"
--   SchemePrimSub -> "-"
--   SchemePrimMul -> "*"
--   SchemePrimDiv -> "/"
--   SchemePrimEq  -> "="

-- schemeRenderParen :: [Text] -> Render Text
-- schemeRenderParen xs =
--   pure $ "(" <> T.intercalate " " xs <> ")"

data RenderState = RenderState
  { stateLevel :: Int
  , stateStyle :: RenderStyle
  , stateText  :: Text
  } deriving (Eq, Ord, Show)

type Render = StateT RenderState IO

render :: Text -> Render ()
render x = modify $ \s -> s { stateText = T.append (stateText s) x }

renderOpen :: Render ()
renderOpen = render "("

renderClose :: Render ()
renderClose = render ")"

renderSpace :: Render ()
renderSpace = render " "

schemeRender :: SchemeExp -> IO Text
schemeRender exp = do
  RenderState{..} <- execStateT (schemeRenderExp exp) $
    RenderState 0 RenderNormal T.empty
  pure stateText

-- schemeRenderProg :: SchemeProg -> Render Text
-- schemeRenderProg (SchemeProg decs) = do
--   d <- forM decs schemeRenderDec
--   pure $ T.intercalate " " d

-- schemeRenderDec :: SchemeDec -> Render Text
-- schemeRenderDec = \case
--   SchemeDecFunc var vars exp -> do
--     undefined
--   SchemeDecDefine var exp -> do
--     undefined
--   SchemeDecBegin decs -> do
--     undefined
--   SchemeDecExp exp -> do
--     undefined

schemeRenderExp :: SchemeExp -> Render ()
schemeRenderExp = \case
  SchemeApp exps -> do
    renderOpen
    forM_ exps $ \e -> do
      renderSpace
      schemeRenderExp e
    renderClose
  SchemeExpLet bindings exp -> do
    schemeRenderLet "let" bindings exp
  SchemeExpLetRec bindings exp -> do
    schemeRenderLet "letrec" bindings exp
  SchemeExpIf exp0 exp1 exp2 -> do
    renderOpen
    render "if"
    renderSpace
    schemeRenderExp exp0
    renderSpace
    schemeRenderExp exp1
    renderSpace
    schemeRenderExp exp2
    renderClose
  SchemeExpSet var exp -> do
    renderOpen
    render "set!"
    renderSpace
    schemeRenderVar var
    renderSpace
    schemeRenderExp exp
    renderClose
  SchemeExpCallCC exp -> do
    renderOpen
    render "call/cc"
    renderSpace
    schemeRenderExp exp
    renderClose
  SchemeExpLam vars exp -> do
    renderOpen
    render "λ"
    renderSpace
    renderOpen
    forM_ vars $ \v -> do
      renderSpace
      schemeRenderVar v
    renderClose
    renderSpace
    schemeRenderExp exp
    renderClose
  SchemeExpVar var -> do
    schemeRenderVar var
  SchemeExpInt x -> do
    render $ T.pack $ show x
  SchemeExpFloat x -> do
    render $ T.pack $ show x
  SchemeExpBool True -> do
    render "#t"
  SchemeExpBool False -> do
    render "#f"
  SchemeExpStr x -> do
    render "\""
    render x
    render "\""
  SchemeExpPrim prim exps -> do
    renderOpen
    schemeRenderPrim prim
    forM_ exps $ \e -> do
      renderSpace
      schemeRenderExp e
    renderClose
  SchemeExpVoid -> do
    render "#<void>"

schemeRenderLet :: Text -> [SchemeBind] -> SchemeExp -> Render ()
schemeRenderLet name bindings exp = do
    renderOpen
    render name
    renderSpace
    renderOpen
    forM_ bindings $ \b -> do
      renderSpace
      schemeRenderBind b
    renderClose
    renderSpace
    schemeRenderExp exp
    renderClose

schemeRenderBind :: SchemeBind -> Render ()
schemeRenderBind (SchemeBind var exp) = do
  renderOpen
  schemeRenderVar var
  renderSpace
  schemeRenderExp exp
  renderClose

schemeRenderVar :: SchemeVar -> Render ()
schemeRenderVar (SchemeVar name) = render name

schemeRenderPrim :: SchemePrim -> Render ()
schemeRenderPrim = render . \case
  SchemePrimAdd -> "+"
  SchemePrimSub -> "-"
  SchemePrimMul -> "*"
  SchemePrimDiv -> "/"
  SchemePrimEq  -> "="

