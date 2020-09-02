{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Tensor where

import qualified LLVM.AST as LLVM
import qualified Data.Map as M
import Control.Monad
import qualified LLVM.IRBuilder as IR
import qualified LLVM.AST.Type as T

-- Types
data Storage = Dense | Sparse deriving (Ord, Eq, Show)
type Index = LLVM.Name 
type Label = LLVM.Name
type Var = LLVM.Name 
data Type = TT LLVM.Type [Storage] deriving (Ord, Eq, Show)
type TypeError = String
type TypeContext = M.Map Var Type 
data Function where
  Function ∷ Label → [(Var, Type)] → Expr → Function

data Expr where
  Let ∷ Var → Expr → Expr → Expr
  Λ ∷ Storage → Index → Expr → Expr
  Σ ∷ Index → Expr → Expr
  Access ∷ Expr → Index → Expr
  Literal ∷ Double → Expr
  Mul ∷ Expr → Expr → Expr
  Add ∷ Expr → Expr → Expr
  Var ∷ Var → Expr
  deriving (Show)

instance Num Expr where
  fromInteger = Literal . fromIntegral
  (*) = Mul
  (+) = Add 
  abs = undefined
  signum = undefined
  (-) = undefined

-- Example functions
examples :: [Function] 
examples = [
 Function "vsmul" [(a, ft []), (b, ft [d])] $
  Λ d i $ v a * b!i, 
 Function "vvmul" [(a, ft [d]), (b, ft [d])] $
  Σ i $ a!i * b!i,
 Function "mvmul" [(a, ft [d, d]), (x, ft [d])] $
  Λ d j (Σ i (a!j!i * x!i)),
 Function "transpose" [(a, ft [d, d])] $
  Λ d i $ Λ d j $ a!j!i,
 Function "t3v" [(b, ft [d,d,d]), (c, ft [d])] $ 
  Λ d i $ Λ d j $ Σ k $ b!i!j!k * c!k
  ]

-- Type inference
equalScalarTypes ∷ Type → Type → Either TypeError Type
equalScalarTypes (TT at []) (TT bt []) = if at == bt then pure $ TT at [] else Left "TODO: type coersion for scalar operations"
equalScalarTypes at bt = Left $ "expected two scalars, got: "  ++ show (at, bt)

tiFun ∷ Function → Either TypeError Type
tiFun (Function _ args body) = ti (M.fromList args) body

ti ∷ TypeContext → Expr → Either TypeError Type
ti c e = case e of
  Let v e b → ti c e >>= \t → ti (M.insert v t c) b
  Σ i e → ti c e
  Λ s i e → ti c e >>= \(TT et ss) → pure $ TT et (s:ss) 
  Access e i → ti c e >>= \(TT et (s:ss)) → pure $ TT et ss
  Mul a b → equalScalarTypes <$> ti c a <#> ti c b 
  Add a b → equalScalarTypes <$> ti c a <#> ti c b 
  Var v → maybe (Left $ "unbound variable: " ++ show v) pure $ M.lookup v c

-- Notational conveniences
class Accessible a where
  (!) :: a → Index → Expr
instance Accessible Var where
  (!) a b = Access (Var a) b
instance Accessible Expr where
  (!) = Access
infixl 9 !
i,j,k ∷ Index
i = "i"; j = "j"; k = "k"; l = "l"
a,b,c ∷ Var
a = "a"; b = "b"; c = "c"; x = "x"
d = Dense; s = Sparse
v = Var
ft = TT T.float 
f <#> x = join $ f <*> x
infixl 4 <#>
