# obf0

An educational exploration into the mutation of LLVM IR to confuse disassemblers, reverse engineers, and also myself.

Features
- rudimentary opaque predicates (trivially reducable by anyone with eyes) and (mostly sound) control flow obfuscation
- MBA substitution (only one substitution for + and - respectively)

This project has only been tested on LLVM 20 and LLVM 21.
