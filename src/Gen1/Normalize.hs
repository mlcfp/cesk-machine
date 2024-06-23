--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Normalize
  ( schemeNormalize
  ) where

import Prelude hiding (exp)
import Control.Monad.State (StateT(..), evalStateT, get, put)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Gen0.ANF as A
import Gen1.Scheme

data State = State
  { stateSymCount :: Int
  } deriving (Eq, Ord, Show)

stateEmpty :: State
stateEmpty = State 0

type Norm = StateT State IO




-- ;; Expression normalization:
-- (define (normalize-term exp) (normalize exp (λ (x) x)))

-- (define (normalize exp k)
--   (match exp
--     [`(λ ,params ,body)
--       (k `(λ ,params ,(normalize-term body)))]

--     [`(let () ,exp)
--       (normalize exp k)]

--     [`(let ([,x ,exp1] . ,clause) ,exp2)
--       (normalize exp1 (λ (aexp1)
--        `(let ([,x ,aexp1])
--          ,(normalize `(let (,@clause) ,exp2) k))))]

--     [`(if ,exp1 ,exp2 ,exp3)
--       (normalize-name exp1 (λ (t)
--        (k `(if ,t ,(normalize-term exp2)
--                   ,(normalize-term exp3)))))]

--     [`(set! ,v ,exp)
--       (normalize-name exp (λ (t)
--        `(let ([,(gensym '_) (set! ,v ,t)])
--           ,(k '(void)))))]

--     [`(,f . ,e*)
--       (normalize-name f (λ (t)
--        (normalize-name* e* (λ (t*)
--         (k `(,t . ,t*))))))]

--     [(? atomic?)
--      (k exp)]))

-- (define (normalize-name exp k)
--   (normalize exp (λ (aexp)
--     (if (atomic? aexp) (k aexp)
--         (let ([t (gensym)])
--          `(let ([,t ,aexp]) ,(k t)))))))

-- (define (normalize-name* exp* k)
--   (if (null? exp*)
--       (k '())
--       (normalize-name (car exp*) (λ (t)
--        (normalize-name* (cdr exp*) (λ (t*)
--         (k `(,t . ,t*))))))))


-- ;; Top-level normalization:
-- (define (normalize-define def)
--   (match def
--     [`(define (,f . ,params) ,body)
--      `(define ,f ,(normalize-term `(λ ,params ,body)))]

--     [`(define ,v ,exp)
--      `(begin ,@(flatten-top (normalize-term exp) v))]))


-- (define (flatten-top exp v)
--   (match exp
--     [`(let ([,x ,cexp]) ,exp)
--      (cons `(define ,x ,cexp)
--             (flatten-top exp v))]

--     [else
--      `((define ,v ,exp))]))


-- (define (normalize-program decs)
--   (match decs
--     ['()
--      '()]

--     [(cons `(define . ,_) rest)
--      (cons (normalize-define (car decs))
--            (normalize-program rest))]

--     [(cons exp rest)
--      (cons (normalize-term exp)
--            (normalize-program rest))]))




-- | Generates a new variable.
gensym :: Norm SchemeVar
gensym = gensym' "g"

-- | Generates a new variable with a prefix.
gensym' :: Text -> Norm SchemeVar
gensym' prefix = do
  State n <- get
  put $ State $ n + 1
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
  SchemeExpPrim  {} -> True
  _otherwise        -> False

-- | Normalizes a term.
normalizeTerm :: SchemeExp -> Norm SchemeExp
normalizeTerm exp = normalize exp pure

-- | Normalizes an expression.
normalize :: SchemeExp -> (SchemeExp -> Norm SchemeExp) -> Norm SchemeExp
normalize exp k = case exp of
  SchemeExpLam vars e -> do
    body <- normalizeTerm e
    k $ SchemeExpLam vars body

  SchemeApp (e:es) -> do
    normalizeName e $ \t -> do
      normalizeNames es $ \ts -> do
        k $ SchemeApp (t:ts)

  SchemeExpLet ((SchemeBind v e1):bs) e2 -> do
    normalize e1 $ \t -> do
      body <- normalize (SchemeExpLet bs e2) k
      pure $ SchemeExpLet [SchemeBind v t] body

  SchemeExpLetRec ((SchemeBind v e1):bs) e2 -> do
    normalize e1 $ \t -> do
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
      body <- k SchemeExpVoid
      pure $ SchemeExpLet [SchemeBind g $ SchemeExpSet v t] body

  SchemeExpCallCC e -> do
    body <- normalizeTerm e
    k $ SchemeExpCallCC body

  _ | atomic exp -> do
    k exp

  _ ->
    pure SchemeExpVoid

-- | Normalizes a name in a scheme expression.
normalizeName :: SchemeExp -> (SchemeExp -> Norm SchemeExp) -> Norm SchemeExp
normalizeName exp k =
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
normalizeNames exps k =
  case exps of
    [] -> k []
    (e:es) -> do
      normalizeName e $ \t -> do
        normalizeNames es $ \ts -> do
          k (t:ts)

-- (define (normalize-define def)
--   (match def
--     [`(define (,f . ,params) ,body)
--      `(define ,f ,(normalize-term `(λ ,params ,body)))]

--     [`(define ,v ,exp)
--      `(begin ,@(flatten-top (normalize-term exp) v))]))

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
-- (define (flatten-top exp v)
--   (match exp
--     [`(let ([,x ,cexp]) ,exp)
--      (cons `(define ,x ,cexp)
--             (flatten-top exp v))]
--     [else
--      `((define ,v ,exp))]))

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

  SchemeProg ((SchemeDecBegin binds):decs) -> do
    error "top level begin not allowed"

  SchemeProg ((SchemeDecExp exp):decs) -> do
    e <- SchemeDecExp <$> normalizeTerm exp
    SchemeProg ds <- normalizeProgram (SchemeProg decs)
    pure $ SchemeProg $ e : ds


-- (define (normalize-program decs)
--   (match decs
--     ['()
--      '()]

--     [(cons `(define . ,_) rest)
--      (cons (normalize-define (car decs))
--            (normalize-program rest))]

--     [(cons exp rest)
--      (cons (normalize-term exp)
--            (normalize-program rest))]))


-- | Normalizes a scheme expression.
schemeNormalize :: SchemeExp -> IO SchemeExp
schemeNormalize exp = evalStateT (normalizeTerm exp) stateEmpty
