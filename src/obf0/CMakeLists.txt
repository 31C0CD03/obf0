target_sources(obf0
    PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/obf0.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/secpol.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/utility/llvm.cpp
)

target_include_directories(obf0
    PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/..
)

add_subdirectory(passes)