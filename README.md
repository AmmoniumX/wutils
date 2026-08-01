# wutils

A best-effort C++20 and above library for converting platform-dependent wide strings (`wchar_t`) to fixed-length Unicode types (`char8_t` `char16_t`, `char32_t`), as well as a platform-independent `wswidth` function (tested to be working and reliable on Windows and Linux systems).

-----

### What It Is

**wutils** is a C++ library that helps you convert system-defined `wchar_t` and `std::wstring` to and from Unicode, fixed-length `charX_t` and `std::uXstring`. It addresses the issue where low-level system calls or libraries use wide strings but you want to use fixed-length unicode strings.

The library provides a "best-effort" conversion by offering consistent type aliases `uchar_t`, `ustring`, and `ustring_view` for their fixed-length Unicode `charX_t` equivalents.

Conversion functions are easy to use, simply call `wutils::u8<std::wstring>(w)` to perform a checked conversion from wstring to u8string, you can convert to and from any combination of `std::string`/`std::wstring`/`std::u8string`/`std::u16string`/`std::u32string`.

-----

### How It Works

**wutils** inspects the size of `wchar_t` at compile time to determine the correct type mapping.

This means:

  * If `sizeof(wchar_t)` is 2 bytes, it assumes a UTF-16 encoding and maps the type aliases to `char16_t`.
  * If `sizeof(wchar_t)` is 4 bytes, it assumes a UTF-32 encoding and maps the type aliases to `char32_t`.

This allows your code to use a consistent `uchar_t`, `ustring`, and `ustring_view` without needing platform-specific conditional compilation.
Wutils also adds a simple conversion between `std::string` and `std::u8string`, based on the assumption that both have the same internal representation.

This allows you to directly convert a `std::wstring` into a `ustring` or a `std::string` to a `std::u8string`, but what if you wanted to convert it into one specific UTF string type, such as `std::u32string`?

To achieve this, wutils allows conversion from and to any supported string type easily using the following conversion strategy:

<img width="480" height="360" alt="wutils-conversion-strategy" src="https://github.com/user-attachments/assets/873db7f7-335c-49aa-a543-60cc174c7423" />

For example, if you are on a Windows (where `ustring = std::u16string`) sytem and wanted to convert a `std::wstring` into a `std::u32string`, wutils would do the following conversions:
1. Convert `std::wstring` into `std::u16string` using implicit conversion (static casting, "fast" conversion).
2. Convert `std::u16string` into `std::u32string` using the internal `std::u32string u32(std::u16string_view)` function.

You can even convert `std::wstring` to and from `std::string`, by converting `std::wstring -> std::u16string -> std::u8string -> std::string`. Again, two of those steps are effectively static casts, so converting them has minimal performance impact, the only "true" conversion happens between UTF16->UTF8.

The library automatically "dispatches" the conversion order based on template overloads, which means that the conversion strategy is resolved during compile-time and the library doesn't have to resolve the correct conversion order during runtime.

The library also includes a platform-independent `uswidth` and `wswidth` functions. These calculate the number of columns a character occupies on a display, which is important for handling characters that take up more than one column, such as CJK ideographs.

-----

### Notes, Assumptions, and Limitations

The C++ standard does not guarantee that `wchar_t` and `std::wstring` are encoded as UTF16 or UTF32. **wutils** makes a critical assumption based on the size of the type.

This can lead to incorrect behavior in certain edge cases and target systems. However, in practice, Windows uses UTF16, while Linux, MacOS, and most UNIX-like systems use UTF32.

If you are using the MSVC compiler on Windows, make sure that you are using the `/utf8` flag to encode `char` as UTF8 (it's set by default when creating a Visual Studio project).

-----

# Getting Started

## As a CMake subdirectory

Add this repo as a submodule (or `FetchContent`) and pull in one of two targets, depending on
whether you want the C++20 module:

* `wutils_header` — plain static library, no C++20 modules involved. Builds on any CMake
  generator.

  ```cmake
  add_subdirectory(wutils)
  target_link_libraries(your_target PRIVATE wutils_header)
  ```

  ```cpp
  #include <wutils/wutils.hpp>
  ```

* `wutils` — same implementation, plus the `wutils.cppm` module interface. Requires a
  modules-capable generator (Ninja, Ninja Multi-Config, or Visual Studio 17.4+ — CMake will error
  out with a clear message if you configure this target with anything else).

  ```cmake
  add_subdirectory(wutils)
  target_link_libraries(your_target PRIVATE wutils)
  target_compile_definitions(your_target PRIVATE WUTILS_MODULE)
  ```

  ```cpp
  import wutils;
  ```

## Without CMake

Copy `include/wutils/` and `src/wutils.cpp` into your project, add `include/` to your compiler's
include path, `#include <wutils/wutils.hpp>`, and compile `wutils.cpp` alongside your own
sources.

For a short usage example, see `examples/example.cpp`. For full API coverage, see
`tests/testwutils.cpp`.
