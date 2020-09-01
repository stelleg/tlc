{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Prelude hiding ((!!), sum)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import LLVM.AST.IntegerPredicate
import LLVM.AST.AddrSpace
import LLVM.AST.Type
import LLVM.IRBuilder
import LLVM.Context
import qualified LLVM as L
import LLVM.Pretty
import qualified Data.Text.Lazy.IO as DT
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString.Char8 as BS
import Debug.Trace(trace)

-- Types
data Storage = Dense | Sparse deriving (Ord, Eq, Show)
type Index = LLVM.Name 
type Label = LLVM.Name
type Var = LLVM.Name 
data TensorType = TT Type [Storage] deriving (Ord, Eq, Show)
type TypeError = String
type TypeContext = M.Map Var TensorType 
data Function where
  Function ∷ Label → [(Var, TensorType)] → TensorType → Expr → Function

data Expr where
  Let ∷ Var → TensorType → Expr → Expr → Expr
  Λ ∷ Index → Expr → Expr
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
vsmul ∷ Function 
vsmul = Function "vsmul" [(a, ftt []), (b, ftt [Dense])] (ftt [Dense]) $
  Λ i $ v a * b!i

vvmul ∷ Function 
vvmul = Function "vvmul" [(a, ftt [Dense]), (b, ftt [Dense])] (ftt []) $
  Σ i $ a!i * b!i

mvmul ∷ Function 
mvmul = Function "mvmul" [(a, ftt [Dense, Dense]), (x, ftt [Dense])] (ftt [Dense]) $
  Λ j (Σ i (a!j!i * x!i))

transpose ∷ Function
transpose = Function "transpose" [(a, ftt [Dense, Dense])] (ftt [Dense, Dense]) $
  Λ i $ Λ j $ a!j!i

-- Typechecking
typeCheck ∷ Function → Either TypeError TensorType
typeCheck (Function _ args retTy body) = tc (M.fromList args) retTy body

tc ∷ TypeContext → TensorType → Expr → Either TypeError TensorType
tc c t e = let TT bt s = t in case e of
  Let v t' e b → tc c t' e >> tc (M.insert v t' c) t b
  Σ i e → tc c t e
  Λ i e → case s of
    [] → Left $ "expected scalar, got: " ++ show e
    s:ss → tc c (TT bt ss) e >> pure (TT bt $ s:ss)
  Access e i → tc c (TT bt $ Dense:s) e 
  Mul a b → case s of 
    [] → tc c t a >> tc c t b
    _ → Left $ "only support scalar multiplication, got: " ++ show e
  Add a b → case s of 
    [] → tc c t a >> tc c t b
    _ → Left $ "only support scalar addition, got: " ++ show e
  Literal l → case s of
    [] → Right t
    _ → Left $ "expected higher order tensor, got scalar literal: "  ++ show e
  Var v → case M.lookup v c of
    Just t' → if t' == t then Right t else Left $ 
      "type failure for variable: " ++  show v ++ "\n expected type: " ++ show t ++ "\n got type: " ++ show t'
    Nothing → Left $ "variable " ++ show v ++ " not in scope"

tensorLLVMType ∷ TensorType → LLVM.Type
tensorLLVMType (TT et []) = et  
tensorLLVMType (TT et (Dense:ss)) = ptr $ StructureType True [i64, arrT] where 
  arrT = case tensorLLVMType (TT et ss) of
    PointerType t as → PointerType t as
    t → ptr t

tensorLLVMParameter ∷ (Var, TensorType) → (LLVM.Type, ParameterName)
tensorLLVMParameter (LLVM.Name v, tt) = (tensorLLVMType tt , ParameterName v) where

tensorLLVMOperand ∷ (Var, TensorType) → LLVM.Operand
tensorLLVMOperand (v, tt) = LLVM.LocalReference (tensorLLVMType tt) v

-- TODO: fix this naming convention hack
indexOp ∷ LLVM.Name → LLVM.Operand
indexOp (LLVM.Name i) = LLVM.LocalReference (LLVM.IntegerType 64) (LLVM.Name$i<>"_0")

varOp ∷ TensorType → LLVM.Name → LLVM.Operand
varOp tt (LLVM.Name v) = LLVM.LocalReference (tensorLLVMType tt) (LLVM.Name$v <>"_0")

-- TODO: Make this index merge tree
varsAccessedByIndex ∷ LLVM.Name → Expr → [LLVM.Name]
varsAccessedByIndex i e = let va = varsAccessedByIndex i in case e of 
  Mul a b → va a ++ va b
  Add a b → va a ++ va b
  Access e j | i == j → vars e ++ va e
  Λ j e | i /= j → va e
  Σ j e | i /= j → va e
  _ → []
  where va = varsAccessedByIndex
        vars (Var v) = [v]
        vars (Access e i) = vars e
        vars _ = []
  
-- Compilation
compileExpr ∷ (MonadModuleBuilder m, MonadFix m) => TensorType → LLVM.Operand → Expr → IRBuilderT m LLVM.Operand
compileExpr tt@(TT et ss) o e = case e of 
  Σ i e → do 
    h ← currentBlock
    loop i (int64 10) o $ \acc → fadd acc =<< compileExpr tt acc e
  Λ i e → do 
    n ← deref $ gep o [int64 0, int32 0]
    l ← deref $ gep o [int64 0, int32 1]
    loop i n o $ \_ → do
      aa ← gep l [indexOp i]
      a ← load aa 0
      v ← compileExpr (TT et$tail ss) a e
      when (length ss == 1) $ store aa 0 v
      return o
    return o
  Access e i → do
    e' ← compileExpr (TT et$Dense:ss) o e
    l ← deref $ gep e' [int64 0, int32 1]
    l' ← gep l [indexOp i]
    load l' 0
  Mul a b → fmul <$> compileExpr tt o a <#> compileExpr tt o b 
  Add a b → fadd <$> compileExpr tt o a <#> compileExpr tt o b
  Var v → pure $ varOp tt v

compile ∷ Function → LLVM.Module
compile (Function name args retTy body) = buildModule "tensor module" $ mdo
  --syncregionstart ← global "llvm.syncregion.start" (FunctionType TokenType [] False) C.TokenNone
  function name (map tensorLLVMParameter $ ("res",retTy):args) (tensorLLVMType retTy) $ \(res:_) -> mdo
    entry ← named block "entry"
    v ← compileExpr retTy res body
    ret v 

loop ∷ (MonadModuleBuilder m, MonadFix m) => LLVM.Name → LLVM.Operand → LLVM.Operand → (LLVM.Operand → IRBuilderT m LLVM.Operand) → IRBuilderT m LLVM.Operand
loop (LLVM.Name i) n o body = mdo 
  startb ← currentBlock 
  start ← pure $ int64 0 
  br cond
  cond ← named block "cond"
  acc ← phi [(o, startb), (res, incb)]
  i' ← phi [(start, startb), (inc, incb)] `named` i  
  ci ← icmp ULT i' n
  condBr ci bodyb exit
  bodyb ← named block "body"
  res ← body acc
  br incb
  incb ← named block "inc"
  inc ← add i' (int64 1)
  br cond
  exit ← named block "exit"
  return acc

-- Notational conveniences
class Accessible a where
  (!) :: a → Index → Expr
instance Accessible Var where
  (!) a b = Access (Var a) b
instance Accessible Expr where
  (!) = Access
infixl 8 !
i,j,k ∷ Index
i = "i"; j = "j"; k = "k"; l = "l"
a,b,c ∷ Var
a = "a"; b = "b"; c = "c"; d = "d"; x = "x"
sum = Σ
v = Var
ftt = TT float 
f <#> x = join $ f <*> x
deref v = flip load 0 =<< v
infixl 4 <#>

main ∷ IO () 
main = do
  DT.writeFile "vvmul.ll" $ ppllvm $ compile vvmul
  DT.writeFile "vsmul.ll" $ ppllvm $ compile vsmul
  --withContext $ \c → L.withModuleFromAST c (compile vvmul) $ \m → L.moduleLLVMAssembly m >>= BS.putStrLn
