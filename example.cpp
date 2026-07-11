#include <cassert>
#include <iostream>
#include <string>
#include <string_view>

#ifdef WUTILS_MODULE
import wutils;
#else
#include "wutils.hpp"
#endif

// Define functions that use "safe" UTF encoded string types
void do_something(std::u8string u8s) { (void)u8s; }
void do_something(std::u16string u16s) { (void)u16s; }
void do_something(std::u32string u32s) { (void)u32s; }
void do_something_u32(std::u32string u32s) { (void)u32s; }
void do_something_w(std::wstring ws) { (void)ws; }

int main() {
  using wutils::ustring; // Type resolved at compile time based on
                         // sizeof(wchar), either std::u16string or
                         // std::u32string

  std::wstring wstr = L"Hello, World";
  ustring ustr = wutils::ws_to_us(wstr); // Convert to UTF string type

  do_something(ustr); // Call our "safe" function using the
                      // implementation-native UTF string equivalent type

  // You can still convert it back to a wstring to use with other APIs
  std::wstring w_out = wutils::us_to_ws(ustr);
  do_something_w(w_out);

  // You can also do a checked conversion to specific UTF string types
  // (see wutils.hpp for explanation of return type)
  wutils::ConversionResult<std::u32string> conv =
      wutils::u32s(wstr, wutils::ErrorPolicy::UseReplacementCharacter);

  if (conv) {
    do_something_u32(*conv);
  }

  // You can also use wutils::string, which is a wrapper around std::string that
  // handles all conversions automatically
  wutils::string s = wutils::string("Hello, World");
  do_something(std::u8string(s));
  do_something(std::u16string(s));
  do_something(std::u32string(s));
  std::string s_out = std::string(s);
  std::cout << s_out << std::endl;
  std::wstring ws_out = std::wstring(s);
  wutils::wprintln(ws_out);

  // Bonus, cross-platform wchar column width function, based on the "East
  // Asian Width" property of unicode characters
  assert(wutils::wswidth(L"中国人") ==
         6); // Chinese characters are 2-cols wide each
  // Works with emojis too (each emoji is 2-cols wide), and emoji sequence
  // modifiers
  assert(wutils::wswidth(L"😂🌎👨‍👩‍👧‍👦") == 6);

  return EXIT_SUCCESS;
}
