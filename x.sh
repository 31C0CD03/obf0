export LLVM_DIR=/opt/homebrew/opt/llvm

# rm *.ll
rm *.dot
rm *.dylib

cmake -DLT_LLVM_INSTALL_DIR=$LLVM_DIR -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -B build
cmake --build build
cp build/libobf0.dylib .

$LLVM_DIR/bin/clang -O3 -flto -S -emit-llvm test.c -o ./test.ll
$LLVM_DIR/bin/opt -S --load-pass-plugin=libobf0.dylib --passes="balls" --disable-verify < test.ll > test_pass.ll
# $LLVM_DIR/bin/clang -O3 -flto -fpass-plugin=./libobf0.dylib test.c -o ./test
# $LLVM_DIR/bin/opt -passes=dot-cfg -disable-output test_pass.ll
# dot .main.dot -Tpng > main.png
# open main.png
$LLVM_DIR/bin/llc test_pass.ll
clang test_pass.s -o ./test_pass

# open foo.png
