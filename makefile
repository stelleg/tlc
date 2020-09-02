
all: test

test: test.c vsmul.ll vvmul.ll mvmul.ll transpose.ll t3v.ll
	clang -O2 -o $@ $?

vsmul.ll vvmul.ll: Tensor.hs LLVM.hs 
	runhaskell LLVM.hs

clean: 
	rm -f vsmul.ll vvmul.ll test

