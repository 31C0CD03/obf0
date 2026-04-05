# obf0

An educational exploration into the mutation of LLVM IR to confuse disassemblers, reverse engineers, and also myself.

Features

- rudimentary opaque predicates (trivially reducable by anyone with eyes) and (mostly sound) control flow obfuscation
- MBA substitution (only one substitution for + and - respectively)

This project has only been tested on LLVM 20 and LLVM 21.

## Usage

```sh
% export LLVM_DIR=...
% ./tools/build
[ 16%] Building CXX object CMakeFiles/obf0.dir/src/obf0/utility/llvm.cpp.o
[ 33%] Building CXX object CMakeFiles/obf0.dir/src/obf0/passes/opq/opq.cpp.o
[ 50%] Linking CXX shared library libobf0.dylib
[100%] Built target obf0
% ./tools/test
(obf0-mba) obfuscating foo
(obf0-mba)      queuing rewrite (add)   %4 = add nsw i32 %3, 1
(obf0-mba)      queuing rewrite (sub)   %5 = sub nsw i32 %4, 2
(obf0-mba) obfuscating foo
(obf0-mba)      queuing rewrite (add)   %10 = add i32 %5, %7
(obf0-mba)      queuing rewrite (add)   %11 = add i32 %10, %9
(obf0-mba)      queuing rewrite (add)   %13 = add i32 %12, 2
verified
(obf0-opq) obfuscating foo
(obf0-opq)      obfuscating 0 blocks
(obf0-opq) obfuscating foo
(obf0-opq)      obfuscating 0 blocks
verified
(obf0-mba) obfuscating main
(obf0-mba)      queuing rewrite (add)   %9 = add nuw nsw i32 %8, 2
(obf0-mba)      queuing rewrite (add)   %12 = add nuw nsw i32 %8, 1
(obf0-mba) obfuscating main
(obf0-mba)      queuing rewrite (add)   %15 = add i32 %10, %12
(obf0-mba)      queuing rewrite (add)   %16 = add i32 %15, %14
(obf0-mba)      queuing rewrite (add)   %25 = add i32 %20, %22
(obf0-mba)      queuing rewrite (add)   %26 = add i32 %25, %24
verified
(obf0-opq) obfuscating main
(obf0-opq)      obfuscating 2 blocks
(obf0-opq) obfuscating main
(obf0-opq)      obfuscating 8 blocks
verified
5
        1
        2
        3
        4
        5
```
