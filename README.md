# Tensor Language Compiler

Compiles a nameless lambda interpretation of einstein notation as a Haskell EDSL into LLVM IR.

```haskell
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
```
