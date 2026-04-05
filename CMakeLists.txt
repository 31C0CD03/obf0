cmake_minimum_required(VERSION 3.20)
project(obf0 LANGUAGES CXX)

# LLVM FindLibEdit
enable_language(C)
set(LT_LLVM_INSTALL_DIR "/opt/homebrew/opt/llvm" CACHE PATH "LLVM installation prefix")

# Add LLVMConfig.cmake
list(APPEND CMAKE_PREFIX_PATH "${LT_LLVM_INSTALL_DIR}/lib/cmake/llvm/")

find_package(LLVM REQUIRED CONFIG)
if("${LLVM_VERSION_MAJOR}" VERSION_LESS 20)
  message(FATAL_ERROR "Found LLVM ${LLVM_VERSION_MAJOR}, but need LLVM 20 or above")
endif()

set(CMAKE_CXX_STANDARD 17 CACHE STRING "")
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_library(obf0 SHARED)

target_include_directories(obf0
  SYSTEM PRIVATE
  ${LLVM_INCLUDE_DIRS}
)

# allow undefined symbols in shared objects on darwin
target_link_libraries(obf0
  PRIVATE
  "$<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>")

if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(obf0 PRIVATE -fno-rtti)
endif()

add_subdirectory(src/obf0)