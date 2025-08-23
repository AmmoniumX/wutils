#pragma once

#include <wchar.h>
#include <uchar.h>
#include <string>
#include <string_view>
#include <iostream>
#include <ranges>


namespace wutils {

std::u8string u8(const std::u16string_view u16s);
std::u8string u8(const std::u32string_view u32s);

std::u16string u16(const std::u8string_view u8s);
std::u16string u16(const std::u32string_view u32s);

std::u32string u32(const std::u8string_view u8s);
std::u32string u32(const std::u16string_view u16s);

int uswidth(const std::u8string_view u8s);
int uswidth(const std::u16string_view u16s);
int uswidth(const std::u32string_view u32s);

static constexpr bool wchar_is_char8 = sizeof(wchar_t) == sizeof(char8_t); // not used anywhere afaik but still here for completion
static constexpr bool wchar_is_char16 = sizeof(wchar_t) == sizeof(char16_t); // used on Windows
static constexpr bool wchar_is_char32 = sizeof(wchar_t) == sizeof(char32_t); // used on Linux, MacOS, and most UNIX systems

// A sanity check to ensure the size matches an expected type
static_assert(wchar_is_char8 || wchar_is_char16 || wchar_is_char32,
              "Unsupported wchar_t width, expecting 8, 16 or 32 bits");

// Only one type matches
static_assert(
    (wchar_is_char8 + wchar_is_char16 + wchar_is_char32) == 1,
    "Exactly one wchar_t type must match"
);

// Assume char is char8_t
static_assert(sizeof(char) == sizeof(char8_t), "Invalid char type assumption");

// Determine the underlying type for ustring at compile-time

using uchar_t = std::conditional_t<wchar_is_char8, char8_t,
                std::conditional_t<wchar_is_char16, char16_t, char32_t>>;
using ustring = std::basic_string<uchar_t>;
using ustring_view = std::basic_string_view<uchar_t>;


// Final sanity check
static_assert(sizeof(wchar_t) == sizeof(ustring::value_type) && sizeof(wchar_t) == sizeof(ustring_view::value_type), "Invalid wchar_t deduction");

#ifdef _MSC_VER // MSVC-only optimization
inline ustring us(const std::wstring_view ws) {
    return ustring(std::from_range, ws);
}

inline std::wstring ws(const ustring_view us) {
    return std::wstring(std::from_range, us);
}

inline std::u8string us(const std::string_view s) {
    return std::u8string(std::from_range, s);
}
#else // General, standard-compliant implementation
inline ustring us(std::wstring_view ws) {
    return ws | std::ranges::views::transform([](wchar_t wc) {
        return static_cast<uchar_t>(wc);
    }) | std::ranges::to<ustring>();
}

inline std::wstring ws(const ustring_view us) {
    return us | std::ranges::views::transform([](uchar_t uc) {
        return static_cast<wchar_t>(uc);
    }) | std::ranges::to<std::wstring>();
}

inline std::u8string us(const std::string_view s) {
    return s | std::ranges::views::transform([](char c) {
        return static_cast<char8_t>(c);
    }) | std::ranges::to<std::u8string>();
}
#endif

inline int wswidth(const std::wstring_view ws) {
    ustring u = wutils::us(ws);
    return wutils::uswidth(u);
}

// Windows sucks and can't properly print std::wcout to terminal so we use a wrapper
#ifdef _WIN32
void wprint(const std::wstring_view ws);
void wprintln(const std::wstring_view ws);
#else
inline void wprint(const std::wstring_view ws) {
    std::wcout << ws;
}
inline void wprintln(const std::wstring_view ws) {
    std::wcout << ws << std::endl;
}
#endif

} // namespace wutils

