
all: test

vsmul.ll vvmul.ll: tlc.hs
	runhaskell tlc.hs

test: test.c vsmul.ll vvmul.ll
	clang -o test $?

