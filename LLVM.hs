{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module LLVM where

import LLVM.AST hiding (function, Function, Add, Mul)
import qualified LLVM.AST.Constant as C
import LLVM.AST.IntegerPredicate
import LLVM.AST.AddrSpace
import LLVM.AST.Type
import LLVM.IRBuilder
import LLVM.Context
import LLVM.Pretty
import qualified Data.Text.Lazy.IO as DT
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as SBS
import Tensor hiding (Type)
import qualified Tensor
import Debug.Trace(trace)

-- We define tensors as having inline data
tensorLLVMType ∷ Tensor.Type → Type
tensorLLVMType (TT et []) = et  
tensorLLVMType (TT et (s:ss)) = case s of 
  Dense → StructureType True [i64, ptr ch] 
  Sparse → StructureType True [i64, ptr $ StructureType True [i64, ptr $ ch]]
  where ch = tensorLLVMType (TT et ss)

tensorLLVMParameter ∷ (Var, Tensor.Type) → (Type, ParameterName)
tensorLLVMParameter (Name v, tt) = (ptrStruct $ tensorLLVMType tt, ParameterName v) 

ptrStruct t@(StructureType _ _) = ptr t -- Structs must be passed by reference
ptrStruct t = t

tensorLLVMOperand ∷ (Var, Tensor.Type) → Operand
tensorLLVMOperand (v, tt) = LocalReference (tensorLLVMType tt) v

-- TODO: fix this naming convention hack
indexOp ∷ Index → Operand
indexOp (Name i) = LocalReference (IntegerType 64) (Name$i<>"_0")

varOp ∷ Tensor.Type → Var → Operand
varOp tt (Name v) = LocalReference (ptrStruct $ tensorLLVMType tt) (Name$v <>"_0")

-- TODO: Make this index merge tree
varsAccessedByIndex ∷ Index → Expr → [Var]
varsAccessedByIndex i e = let va = varsAccessedByIndex i in case e of 
  Mul a b → va a ++ va b
  Add a b → va a ++ va b
  Access e j | i == j → vars e ++ va e
  Λ s j e | i /= j → va e
  Σ j e | i /= j → va e
  _ → []
  where va = varsAccessedByIndex
        vars (Var v) = [v]
        vars (Access e i) = vars e
        vars _ = []

indexInto ∷ LLVMMonad m ⇒ 
  Operand →
  Operand → 
  IRBuilderT m Operand
indexInto i o@(LocalReference (PointerType (StructureType _ [i64, _]) _)  n) = do
  ge [int64 0, int32 1] o `named` "arr" >>= flip gep [i]
indexInto i o = error $ "invalid o: " ++ show o ++ " for var " ++ show i

-- Compilation
compileExpr ∷ LLVMMonad m ⇒ 
  TypeContext → -- Type environment for free variables in expr
  Operand → -- Operand to write result to
  Expr → -- Expression to compile
  IRBuilderT m Operand -- Returns operand written to
compileExpr c o e = trace (show e ++ show o) $ case e of 
  Σ i e → loop c Nothing i e o
  Λ t i e → loop c (Just t) i e o
  Access e i → compileExpr c o e >>= floatDeref . indexInto (indexOp i) 
  Mul a b → fmul <$> compileExpr c o a <#> compileExpr c o b 
  Add a b → fadd <$> compileExpr c o a <#> compileExpr c o b
  Var v → maybe (error $ "unound variable " ++ show v) (pure . flip varOp v) $ M.lookup v c 
  Let _ _ _ → error "TODO: implement Let"

compile ∷ Function → Either TypeError Module 
compile f@(Function name args body) = tiFun f >>= \retTy → pure $ buildModule "tensor module" $ mdo
  --syncregionstart ← global "llvm.syncregion.start" (FunctionType TokenType [] False) C.TokenNone
  function name (map tensorLLVMParameter $ ("res",retTy):args) (ptrStruct $ tensorLLVMType retTy) $ \(res:_) -> mdo
    entry ← named block "entry"
    v ← compileExpr (M.fromList args) res body 
    ret v 

loop ∷ LLVMMonad m => 
  TypeContext → 
  Maybe Storage → -- Storage type to be written to if Lambda loop
  Name → -- Name of induction variable  
  Expr → -- Body of loop
  Operand → -- Operand to write to
  IRBuilderT m Operand -- Returns operand written to
-- TODO: even if result is dense, target could be sparse 
loop te (Just Dense) (Name i) body o = mdo
  startb ← currentBlock 
  start ← pure $ int64 0 
  n ← getN o 
  br cond
  cond ← named block "cond"
  i' ← phi [(start, startb), (inc, incb)] `named` i 
  ci ← icmp ULT i' n
  condBr ci bodyb exit
  bodyb ← named block "body"
  loc ← indexInto i' o
  res ← compileExpr te loc body
  storeDeref loc res
  br incb
  incb ← named block "inc"
  inc ← add i' (int64 1)
  br cond
  exit ← named block "exit"
  return o
loop te Nothing ni@(Name i) body o = mdo
  startb ← currentBlock 
  o' ← floatDeref $ pure o
  start ← pure $ int64 0
  let (v:vs) = varsAccessedByIndex ni body
  n ← pure $ int64 10
  br cond
  cond ← named block "cond"
  i' ← phi [(start, startb), (inc, incb)] `named` i 
  acc ← phi [(o', startb), (acc', incb)] `named` "acc"
  ci ← icmp ULT i' n
  condBr ci bodyb exit
  bodyb ← named block "body"
  res ← floatDeref $ compileExpr te o body
  acc' ← fadd acc res 
  br incb
  incb ← named block "inc"
  inc ← add i' (int64 1)
  br cond
  exit ← named block "exit"
  return acc 

main ∷ IO () 
main = mapM_ c examples where
  c f@(Function n _ _) = do 
    putStr $ "compiling " ++ ppname n ++ " ... "
    either putStrLn (DT.writeFile (ppname n<>".ll") . ppllvm) . compile $ f
    putStrLn $ "done"

-- Convenience
type LLVMMonad m = (MonadModuleBuilder m, MonadFix m)

storeDeref ∷ LLVMMonad m => Operand → Operand → IRBuilderT m () 
storeDeref l o@(LocalReference (PointerType t _) _) = deref (pure o) >>= store l 0 
storeDeref l o = store l 0 o
  

floatDeref ∷ LLVMMonad m => IRBuilderT m Operand → IRBuilderT m Operand
floatDeref v = f =<< v where
  f v@(LocalReference (PointerType (FloatingPointType _) _) _) = load v 0
  f v = pure v 

deref ∷ LLVMMonad m => IRBuilderT m Operand → IRBuilderT m Operand
deref v = flip load 0 =<< v

ge inds = deref . flip gep inds
ppname (Name n) = BS.foldr (:) "" $ SBS.fromShort n
getN o = deref $ gep o [int64 0, int32 0]
