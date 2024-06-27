--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Normalize
  ( normalizeExp
  , normalizeProg
  ) where

import Prelude hiding (exp)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT(..), evalStateT, get, put)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Gen0.ANF as A
import Gen1.Scheme

-- | Defines the normalizer state.
data NormState = NormState
  { normStateSymCount :: Int
  } deriving (Eq, Ord, Show)

-- | Defines a default normalizer state.
normStateEmpty :: NormState
normStateEmpty = NormState 0

-- | Defines a normalizer monad.
type Norm = StateT NormState IO

-- | Generates a new variable.
gensym :: Norm SchemeVar
gensym = gensym' "g"

-- | Generates a new variable with a prefix.
gensym' :: Text -> Norm SchemeVar
gensym' prefix = do
  NormState n <- get
  put $ NormState $ n + 1
  pure $ SchemeVar $ prefix <> T.pack (show n)

-- | Determines if an expression is atomic.
atomic :: SchemeExp -> Bool
atomic = \case
  SchemeExpLam   {} -> True
  SchemeExpVar   {} -> True
  SchemeExpInt   {} -> True
  SchemeExpFloat {} -> True
  SchemeExpBool  {} -> True
  SchemeExpStr   {} -> True
  _otherwise        -> False

-- | Normalizes a term.
normalizeTerm :: SchemeExp -> Norm SchemeExp
normalizeTerm exp = normalize exp pure

-- | Normalizes an expression.
normalize :: SchemeExp -> (SchemeExp -> Norm SchemeExp) -> Norm SchemeExp
normalize exp k = case exp of
  SchemeExpApp (e:es) -> do
    normalizeName e $ \t -> do
      normalizeNames es $ \ts -> do
        k $ SchemeExpApp (t:ts)

-- [`(let ([,x ,exp1] . ,clause) ,exp2)
--       (normalize exp1 (λ (aexp1)
--        `(let ([,x ,aexp1])
--          ,(normalize `(let (,@clause) ,exp2) k))))]
-- [`(let () ,exp)
--       (normalize exp k)]
  SchemeExpLet [] e -> do
    normalize e k

  SchemeExpLet ((SchemeBind v e1):bs) e2 -> do
    normalize e1 $ \t -> do
      body <- normalize (SchemeExpLet bs e2) k
      pure $ SchemeExpLet [SchemeBind v t] body
  SchemeExpLetRec ((SchemeBind v e1):bs) e2 -> do
    normalize e1 $ \t -> do
      -- TODO: should this be normalized to regular lets?
      body <- normalize (SchemeExpLetRec bs e2) k
      pure $ SchemeExpLetRec [SchemeBind v t] body
  SchemeExpIf exp1 exp2 exp3 -> do
    normalizeName exp1 $ \t -> do
      e2 <- normalizeTerm exp2
      e3 <- normalizeTerm exp3
      k $ SchemeExpIf t e2 e3
  SchemeExpSet v e -> do
    normalizeName e $ \t -> do
      g <- gensym' "_"
      body <- k $ SchemeExpApp [SchemeExpVar $ SchemeVar "void"]
      pure $ SchemeExpLet [SchemeBind g $ SchemeExpSet v t] body
  SchemeExpCallCC e -> do
    body <- normalizeTerm e
    k $ SchemeExpCallCC body
  SchemeExpLam vars e -> do
    body <- normalizeTerm e
    k $ SchemeExpLam vars body
  _otherwise | atomic exp -> do
    k exp
  _otherwise -> do
    liftIO $ putStrLn $ "_otherwise " <> show exp
    pure SchemeExpVoid

-- | Normalizes a name in a scheme expression.
normalizeName :: SchemeExp -> (SchemeExp -> Norm SchemeExp) -> Norm SchemeExp
normalizeName exp k = do
  normalize exp $ \aexp ->
    if | atomic aexp -> do
           k aexp
       | otherwise -> do
           t <- gensym
           e <- k $ SchemeExpVar t
           b <- normalizeTerm aexp
           pure $ SchemeExpLet [SchemeBind t b] e

-- | Normalizes names in a scheme expression.
normalizeNames :: [SchemeExp] -> ([SchemeExp] -> Norm SchemeExp) -> Norm SchemeExp
normalizeNames exps k = do
  case exps of
    [] -> k []
    (e:es) -> do
      normalizeName e $ \t -> do
        normalizeNames es $ \ts -> do
          k (t:ts)

-- | Flattens some top-level structures.
flattenTop :: SchemeExp -> SchemeVar -> Norm [SchemeDec]
flattenTop exp var = case exp of
  SchemeExpLet ((SchemeBind v cexp):[]) exp -> do
    decs <- flattenTop exp var
    pure $ (SchemeDecDefine v cexp) : decs
  SchemeExpLetRec ((SchemeBind v cexp):[]) exp -> do
    decs <- flattenTop exp var
    pure $ (SchemeDecDefine v cexp) : decs
  _otherwise -> do
    pure $ [SchemeDecDefine var exp]

-- | Normalizes a program.
normalizeProgram :: SchemeProg -> Norm SchemeProg
normalizeProgram = \case
  SchemeProg [] ->
    pure $ SchemeProg []
  SchemeProg ((SchemeDecFunc var vars exp):decs) -> do
    lam <- normalizeTerm $ SchemeExpLam vars exp
    SchemeProg ds <- normalizeProgram (SchemeProg decs)
    pure $ SchemeProg $ (SchemeDecDefine var lam) : ds
  SchemeProg ((SchemeDecDefine var exp):decs) -> do
    e <- normalizeTerm exp
    d <- SchemeDecBegin <$> flattenTop e var
    SchemeProg ds <- normalizeProgram (SchemeProg decs)
    pure $ SchemeProg $ d : ds
  SchemeProg ((SchemeDecBegin d):decs) -> do
    SchemeProg d' <- normalizeProgram (SchemeProg d)
    SchemeProg decs' <- normalizeProgram (SchemeProg decs)
    pure $ SchemeProg $ (SchemeDecBegin d') : decs'
  SchemeProg ((SchemeDecExp exp):decs) -> do
    e <- SchemeDecExp <$> normalizeTerm exp
    SchemeProg ds <- normalizeProgram (SchemeProg decs)
    pure $ SchemeProg $ e : ds

-- | Normalizes a scheme program.
normalizeExp :: SchemeExp -> IO SchemeExp
normalizeExp prog = evalStateT (normalizeTerm prog) normStateEmpty

-- | Normalizes a scheme program.
normalizeProg :: SchemeProg -> IO SchemeProg
normalizeProg prog = evalStateT (normalizeProgram prog) normStateEmpty
