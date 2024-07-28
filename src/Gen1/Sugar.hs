--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Sugar
  ( DesugarError(..)
  , desugarBegin
  , desugarErrorHumanize
  , schemeDesugar
  ) where

import Control.Monad ((>=>))
import Data.Text (Text)
import Gen1.Scheme

-- | Defines a desugar error
data DesugarError = DesugarError Text deriving (Eq, Ord, Show)

-- | Converts an error into human friendly form.
desugarErrorHumanize :: DesugarError -> Text
desugarErrorHumanize (DesugarError x) = x

-- | Applies the desugar stage to a scheme program.
schemeDesugar :: SchemeProg -> Either DesugarError SchemeProg
schemeDesugar = desugarBegin

-- | Desugar begin sequences.
-- This transform will replace begin sequences with lambda expressions
-- as described in the paper "Lambda the Ultimate Imperative".
desugarBegin :: SchemeProg -> Either DesugarError SchemeProg
desugarBegin =
  rewriteProg schemeTransformerDefault { transformExp = pure . b }
  where
    b (SchemeExpBegin exps) = expSeq exps
    b x = x

-- | Desugar a sequence of two expressions.
-- The transform from the paper is:
--   ((lamdba (d) s2) s1) == (block s1 s2)
expBlock :: SchemeExp -> SchemeExp -> SchemeExp
expBlock e0 e1 = SchemeExpApp [ SchemeExpLam [SchemeVar "__r"] e1, e0]

-- | Desugar a sequence of expressions.
-- The transform from the paper is:
--   (block s1 (block s2 (block ... (block sn-1 sn)...)))
expSeq :: [SchemeExp] -> SchemeExp
expSeq (e0:e1:es) = expBlock e0 $ expSeq (e1:es)
expSeq (e0:e1:[]) = expBlock e0 e1
expSeq (e0:[]) = e0
expSeq [] = error "empty begin"

-- | Defines a desugaring transformer.
data SchemeTransformer = SchemeTransformer
  { transformProg :: SchemeProg -> Either DesugarError SchemeProg
  , transformDec  :: SchemeDec -> Either DesugarError SchemeDec
  , transformExp  :: SchemeExp -> Either DesugarError SchemeExp
  , transformBind :: SchemeBind -> Either DesugarError SchemeBind
  , transformVar  :: SchemeVar -> Either DesugarError SchemeVar
  }

-- | The default transformer.
schemeTransformerDefault :: SchemeTransformer
schemeTransformerDefault = SchemeTransformer pure pure pure pure pure

-- | Rewrites a program with a given transformer.
rewriteProg :: SchemeTransformer -> SchemeProg -> Either DesugarError SchemeProg
rewriteProg t@SchemeTransformer{..} (SchemeProg decs) =
  SchemeProg <$> mapM (transformDec >=> rewriteDec t) decs

-- | Rewrites a declaration with a given transformer.
rewriteDec :: SchemeTransformer -> SchemeDec -> Either DesugarError SchemeDec
rewriteDec t@SchemeTransformer{..} x = case x of
  SchemeDecFunc var vars exp ->
    SchemeDecFunc
      <$> transformVar var
      <*> mapM transformVar vars
      <*> re exp
  SchemeDecDefine var exp ->
    SchemeDecDefine <$> transformVar var <*> re exp
  SchemeDecBegin decs ->
    SchemeDecBegin <$> mapM (transformDec >=> rewriteDec t) decs
  SchemeDecExp exp ->
    SchemeDecExp <$> re exp
  where
    re = transformExp >=> rewriteExp t

-- | Rewrites an expression with a given transformer.
rewriteExp :: SchemeTransformer -> SchemeExp -> Either DesugarError SchemeExp
rewriteExp t@SchemeTransformer{..} x = case x of
  SchemeExpApp exps ->
    SchemeExpApp <$> mapM re exps
  SchemeExpBegin exps ->
    SchemeExpBegin <$> mapM re exps
  SchemeExpLet binds exp ->
    SchemeExpLet <$> mapM rb binds <*> re exp
  SchemeExpLetRec binds exp ->
    SchemeExpLetRec <$> mapM rb binds <*> re exp
  SchemeExpIf exp0 exp1 exp2 ->
    SchemeExpIf <$> re exp0 <*> re exp1 <*> re exp2
  SchemeExpSet var exp ->
    SchemeExpSet <$> transformVar var <*> re exp
  SchemeExpCallCC exp ->
    SchemeExpCallCC <$> re exp
  SchemeExpLam vars exp ->
    SchemeExpLam <$> mapM transformVar vars <*> re exp
  _otherwise ->
    pure x
  where
    re = transformExp >=> rewriteExp t
    rb = transformBind >=> rewriteBind t

-- | Rewrites a binding with a given transformer.
rewriteBind :: SchemeTransformer -> SchemeBind -> Either DesugarError SchemeBind
rewriteBind t@SchemeTransformer{..} (SchemeBind var exp) =
  SchemeBind <$> transformVar var <*> (transformExp exp >>= rewriteExp t)
