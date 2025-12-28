# Minimum CMake version required
cmake_minimum_required(VERSION 3.28)

# IMPORTANT: Force Ninja generator for C++20 modules
if(NOT CMAKE_GENERATOR STREQUAL "Ninja")
    message(FATAL_ERROR "C++20 modules require the Ninja generator. Use: cmake -G Ninja -B build")
endif()

set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build.")

# Project information
project(TestWutils CXX)

# Set the required C++ standard
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

# Scan for modules
set(CMAKE_CXX_SCAN_FOR_MODULES ON)

# Add compiler-specific flags
# Do NOT set CMAKE_CXX_FLAGS for modules
# CMake will handle module compilation flags automatically
# Only set these for non-module targets
if(MSVC)
    set(CMAKE_CXX_FLAGS "/W4 /WX /EHsc /utf-8")
    set(CMAKE_CXX_FLAGS_DEBUG "/Od /Zi")
    set(CMAKE_CXX_FLAGS_RELEASE "/O2")
elseif(UNIX)
    set(CMAKE_CXX_FLAGS "-Wall -Wextra -march=native")
    set(CMAKE_CXX_FLAGS_DEBUG "-O0 -g")
    set(CMAKE_CXX_FLAGS_RELEASE "-O2")
endif()

# Add doctest
include(FetchContent)
FetchContent_Declare(
  doctest
  GIT_REPOSITORY https://github.com/doctest/doctest.git
  GIT_TAG master
)
FetchContent_MakeAvailable(doctest)

# DON'T add custom compile options to module targets
# Let CMake handle it

# Create a library for the module-based build
add_library(wutils)
target_sources(wutils 
    PUBLIC 
        FILE_SET cxx_modules 
        TYPE CXX_MODULES 
        FILES wutils.cppm
    PRIVATE
        wutils.cpp
)
target_compile_definitions(wutils PRIVATE WUTILS_MODULE)
# DON'T manually set include directories for module outputs

# Add executable target for module-based build
add_executable(testwutils_module testwutils.cpp)
target_link_libraries(testwutils_module PRIVATE wutils doctest)
target_include_directories(testwutils_module PRIVATE ${doctest_SOURCE_DIR}/doctest)
target_compile_definitions(testwutils_module PRIVATE WUTILS_MODULE)

# Add executable target for header-only build
add_executable(testwutils_header testwutils.cpp wutils.cpp)
target_link_libraries(testwutils_header PRIVATE doctest)
target_include_directories(testwutils_header PRIVATE ${doctest_SOURCE_DIR}/doctest)

add_executable(example example.cpp)
target_link_libraries(example PRIVATE wutils)
target_compile_definitions(example PRIVATE WUTILS_MODULE)

# Enable testing with CTest
enable_testing()
add_test(NAME testwutils_module COMMAND testwutils_module)
add_test(NAME testwutils_header COMMAND testwutils_header)
