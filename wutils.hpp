#pragma once

#include <uchar.h>
#include <wchar.h>

#include <string>
#include <string_view>
#include <type_traits>

#include "wutils_internal.hpp"

namespace wutils {

static constexpr bool wchar_is_char8 =
    sizeof(wchar_t) ==
    sizeof(char8_t); // not used anywhere afaik but still here for completion
static constexpr bool wchar_is_char16 =
    sizeof(wchar_t) == sizeof(char16_t); // used on Windows
static constexpr bool wchar_is_char32 =
    sizeof(wchar_t) ==
    sizeof(char32_t); // used on Linux, MacOS, and most UNIX systems

// A sanity check to ensure the size matches an expected type
static_assert(wchar_is_char8 || wchar_is_char16 || wchar_is_char32,
              "Unsupported wchar_t width, expecting 8, 16 or 32 bits");

// Only one type matches
static_assert((wchar_is_char8 + wchar_is_char16 + wchar_is_char32) == 1,
              "Exactly one wchar_t type must match");

// We assume char is char8_t, and treat std::string as UTF8
// Note: on windows using MSVC, make sure you compile with the `/utf8` flag

// Determine the underlying type for ustring at compile-time

using uchar_t =
    std::conditional_t<wchar_is_char8, char8_t,
                       std::conditional_t<wchar_is_char16, char16_t, char32_t>>;
using ustring = std::basic_string<uchar_t>;
using ustring_view = std::basic_string_view<uchar_t>;

// Final sanity check
static_assert(sizeof(wchar_t) == sizeof(ustring::value_type) &&
                  sizeof(wchar_t) == sizeof(ustring_view::value_type),
              "Invalid wchar_t deduction");

namespace detail {

// Concepts

template <typename T, template <typename...> class C>
struct instantiation_of_impl : std::false_type {};

template <template <typename...> class C, typename... Args>
struct instantiation_of_impl<C<Args...>, C> : std::true_type {};

template <typename T, template <typename...> class C>
concept instantiation_of = detail::instantiation_of_impl<T, C>::value;

template <typename T, template <typename...> class... Cs>
concept instantiation_of_one_of = (... or instantiation_of<T, Cs>);

template <typename T>
concept BasicString = instantiation_of<T, std::basic_string>;

template <typename T>
concept BasicStringView =
    instantiation_of_one_of<T, std::basic_string, std::basic_string_view>;

template <typename T>
concept is_unicode_char =
    std::is_same_v<T, char8_t> || std::is_same_v<T, char16_t> ||
    std::is_same_v<T, char32_t>;

// ===== Implicit Conversions =====
template <typename FromChar, typename ToChar>
struct implicit_conversion : std::false_type {};
template <> struct implicit_conversion<char, char8_t> : std::true_type {};

template <> struct implicit_conversion<char8_t, char> : std::true_type {};

template <> struct implicit_conversion<wchar_t, uchar_t> : std::true_type {};

template <> struct implicit_conversion<uchar_t, wchar_t> : std::true_type {};

template <typename CharT>
struct implicit_conversion<CharT, CharT> : std::true_type {};

template <typename From, typename To>
inline constexpr bool is_implicitly_convertible =
    implicit_conversion<From, To>::value;

template <detail::BasicStringView From, detail::BasicString To>
  requires is_implicitly_convertible<typename From::value_type,
                                     typename To::value_type>
To convert_implicitly(From from) {
  if constexpr (std::is_same_v<typename From::value_type,
                               typename To::value_type>) {
    return To(from);
  } else {

    // Since both are character types, reinterpreting between both pointer types
    // is valid under strict aliasing rules
    return To(reinterpret_cast<typename To::const_pointer>(from.data()),
              from.length());
  }
};

} // namespace detail
using detail::BasicString, detail::BasicStringView;

// "Dispatch" our functions based on conversion type //

// OVERLOAD 1: Implicit conversion (fast path).
template <BasicStringView From, BasicString To>
  requires(detail::is_implicitly_convertible<typename From::value_type,
                                             typename To::value_type>)
inline ConversionResult<To> convert(From from,
                                    [[maybe_unused]] ErrorPolicy errorPolicy =
                                        ErrorPolicy::UseReplacementCharacter) {
  return {detail::convert_implicitly<From, To>(from), true};
}

// OVERLOAD 2: The "Unicode Kernel." Both types are different Unicode formats.
template <BasicStringView From, BasicString To>
  requires(!detail::is_implicitly_convertible<typename From::value_type,
                                              typename To::value_type> &&
           detail::is_unicode_char<typename From::value_type> &&
           detail::is_unicode_char<typename To::value_type>)
inline ConversionResult<To>
convert(From from,
        ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  using ToChar = typename To::value_type;
  if constexpr (std::is_same_v<ToChar, char8_t>) {
    return detail::u8(from, errorPolicy);
  } else if constexpr (std::is_same_v<ToChar, char16_t>) {
    return detail::u16(from, errorPolicy);
  } else if constexpr (std::is_same_v<ToChar, char32_t>) {
    return detail::u32(from, errorPolicy);
  }
}

// OVERLOAD 3: Entry point. Convert non-Unicode source to a Unicode pivot and
// recurse.
template <BasicStringView From, BasicString To>
  requires(!detail::is_unicode_char<typename From::value_type> &&
           !detail::is_implicitly_convertible<typename From::value_type,
                                              typename To::value_type>)
inline ConversionResult<To>
convert(From from,
        ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  if constexpr (std::is_same_v<typename From::value_type, char>) {
    // Source is char-based. Pivot through u8string.
    return convert<std::u8string, To>(
        detail::convert_implicitly<From, std::u8string>(from), errorPolicy);
  } else if constexpr (std::is_same_v<typename From::value_type, wchar_t>) {
    // Source is wchar_t-based. Pivot through ustring.
    return convert<ustring, To>(detail::convert_implicitly<From, ustring>(from),
                                errorPolicy);
  }
}

// OVERLOAD 4: Exit point. Source is Unicode, destination is not.
template <BasicStringView From, BasicString To>
  requires(!detail::is_implicitly_convertible<typename From::value_type,
                                              typename To::value_type> &&
           detail::is_unicode_char<typename From::value_type> &&
           !detail::is_unicode_char<typename To::value_type>)
inline ConversionResult<To>
convert(From from,
        ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  if constexpr (std::is_same_v<typename To::value_type, char>) {
    // Destination is char-based. Pivot through u8string.
    ConversionResult<std::u8string> intermediate =
        convert<From, std::u8string>(from, errorPolicy);
    return {detail::convert_implicitly<std::u8string, To>(intermediate.value),
            intermediate.is_valid};
  } else if constexpr (std::is_same_v<typename To::value_type, wchar_t>) {
    // Destination is wchar_t-based. Pivot through ustring.
    ConversionResult<ustring> intermediate =
        convert<From, ustring>(from, errorPolicy);
    return {detail::convert_implicitly<ustring, To>(intermediate.value),
            intermediate.is_valid};
  }
}

// Simple conversions to avoid ConversionResult
inline ustring ws_to_us(std::wstring_view from) {
  return detail::convert_implicitly<std::wstring_view, ustring>(from);
}

inline std::wstring us_to_ws(ustring_view from) {
  return detail::convert_implicitly<ustring_view, std::wstring>(from);
}

inline std::u8string s_to_u8s(std::string_view from) {
  return detail::convert_implicitly<std::string_view, std::u8string>(from);
}

inline std::string u8s_to_s(std::u8string_view from) {
  return detail::convert_implicitly<std::u8string_view, std::string>(from);
}

// "Advanced" conversions that require ConversionResult
template <BasicStringView From>
inline ConversionResult<std::u8string>
u8s(From from, ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  return convert<From, std::u8string>(from, errorPolicy);
}

template <BasicStringView From>
inline ConversionResult<std::u16string>
u16s(From from,
     ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  return convert<From, std::u16string>(from, errorPolicy);
}

template <BasicStringView From>
inline ConversionResult<std::u32string>
u32s(From from,
     ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  return convert<From, std::u32string>(from, errorPolicy);
}

template <BasicStringView From>
inline ConversionResult<ustring>
us(From from, ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  return convert<From, ustring>(from, errorPolicy);
}

template <BasicStringView From>
inline ConversionResult<std::wstring>
ws(From from, ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  return convert<From, std::wstring>(from, errorPolicy);
}

template <BasicStringView From>
inline ConversionResult<std::string>
s(From from, ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter) {
  return convert<From, std::string>(from, errorPolicy);
}

inline int wswidth(const std::wstring_view ws) {
  ustring u = wutils::ws_to_us(ws);
  return wutils::uswidth(u);
}

} // namespace wutils
