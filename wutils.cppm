module;

#include "wutils.hpp"

export module wutils;

export namespace wutils {
// Define inline wcout/wcerr if not on windows
using ::wutils::wcerr;
using ::wutils::wcout;

using ::wutils::wprint;
using ::wutils::wprintln;

// Export types and enums from wutils_internal.hpp
using ::wutils::ConversionResult;
using ::wutils::ErrorPolicy;

using ::wutils::wchar_is_char16;
using ::wutils::wchar_is_char32;
using ::wutils::wchar_is_char8;

using ::wutils::uchar_t;
using ::wutils::ustring;
using ::wutils::ustring_view;

namespace detail {
// Export replacement character constants
using ::wutils::detail::REPLACEMENT_CHAR_16;
using ::wutils::detail::REPLACEMENT_CHAR_32;
using ::wutils::detail::REPLACEMENT_CHAR_8;

using ::wutils::detail::BasicString;
using ::wutils::detail::BasicStringView;

} // namespace detail

using detail::BasicString, detail::BasicStringView;

using ::wutils::convert;
using ::wutils::s_to_u8s;
using ::wutils::u8s_to_s;
using ::wutils::us_to_ws;
using ::wutils::ws_to_us;
using ::wutils::detail::convert_implicitly;

using ::wutils::s;
using ::wutils::u16s;
using ::wutils::u32s;
using ::wutils::u8s;
using ::wutils::us;
using ::wutils::ws;

// Export uswidth overloads
using ::wutils::uswidth;
using ::wutils::wswidth;

} // namespace wutils
