#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <array>
#include <string>

#include "doctest.h"

// Import wutils as either module or header
#ifdef WUTILS_MODULE
import wutils;
#else
#include "wutils.hpp"
#endif

using namespace std::string_literals;

struct InputData {
  int width;
  std::u8string text;
};

std::array test_data{InputData{13, u8"Hello, World!"},
                     InputData{6, u8"Résumé"},
                     InputData{6, u8"😂😂😂"},
                     InputData{0, u8""},
                     InputData{2, u8"👩🏼‍🚀"},
                     InputData{4, u8"𐌀𐌍𐌓𐌀"},
                     InputData{11, u8"𝕄𝕒𝕥𝕙𝕖𝕞𝕒𝕥𝕚𝕔𝕤"},
                     InputData{6, u8"🌍🌎🌏"},
                     InputData{2, u8"👨‍👩‍👧‍👦"},
                     InputData{10, u8"𠔻𠕋𠖊𠖍𠖐"},
                     InputData{2, u8"𠮷"},
                     InputData{6, u8"𠀤𠀧𠁀"},
                     InputData{4, u8"𠊛好"},
                     InputData{6, u8"𪚥𪆷𪃹"},
                     InputData{6, u8"𪜈𪜋𪜌"},
                     InputData{7, u8"اَلْعَرَبِيَّةُ"},
                     InputData{0, u8"\x04"},      // End of Transmission (EOT)
                     InputData{0, u8"\x04\x05"}}; // EOT + Enquiry (ENQ)

TEST_CASE("Width calculation") {
  for (const auto &[width, text] : test_data) {
    SUBCASE(("Testing width of: " + std::string(text.begin(), text.end()))
                .c_str()) {
      CHECK(wutils::uswidth(text) == width);
    }
  }
}

TEST_CASE("String conversions") {
  for (const auto &[width, u8s] : test_data) {
    SUBCASE(("Testing conversions for: " + std::string(u8s.begin(), u8s.end()))
                .c_str()) {
      // u8 -> u16 -> u8
      auto u16s = wutils::u16s(u8s);
      REQUIRE(u16s);
      CHECK(wutils::uswidth(*u16s) == width);
      auto u8s_from_u16s = wutils::u8s(*u16s);
      REQUIRE(u8s_from_u16s);
      CHECK(*u8s_from_u16s == u8s);

      // u8 -> u32 -> u8
      auto u32s = wutils::u32s(u8s);
      REQUIRE(u32s);
      CHECK(wutils::uswidth(*u32s) == width);
      auto u8s_from_u32s = wutils::u8s(*u32s);
      REQUIRE(u8s_from_u32s);
      CHECK(*u8s_from_u32s == u8s);

      // u8 -> wstring -> u8
      auto ws = wutils::ws(u8s);
      REQUIRE(ws);
      CHECK(wutils::wswidth(*ws) == width);
      auto u8s_from_ws = wutils::u8s(*ws);
      REQUIRE(u8s_from_ws);
      CHECK(*u8s_from_ws == u8s);

      // u8 -> string -> u8
      auto s = wutils::s(u8s);
      REQUIRE(s);
      auto u8s_from_s = wutils::u8s(*s);
      REQUIRE(u8s_from_s);
      CHECK(*u8s_from_s == u8s);
    }

    SUBCASE(("Testing implicit conversions with wutils::string for:" +
             std::string(u8s.begin(), u8s.end()))
                .c_str()) {

      wutils::string wstr(u8s);

      auto u8 = wutils::u8s(wstr);
      REQUIRE(u8);
      CHECK(*u8 == u8s);
      CHECK(wutils::uswidth(*u8) == width);

      auto u16 = wutils::u16s(wstr);
      REQUIRE(u16);
      CHECK(u16 == wutils::u16s(u8s));
      CHECK(wutils::uswidth(*u16) == width);

      auto u32 = wutils::u32s(wstr);
      REQUIRE(u32);
      CHECK(u32 == wutils::u32s(u8s));
      CHECK(wutils::uswidth(*u32) == width);
    }
  }
}

TEST_CASE("Error Handling") {
  SUBCASE("Invalid UTF-8 sequences") {
    const char8_t invalid_u8_data[] = {
        's',  't', 'a', 'r', 't', '_', 0xC0, 0xAF, // Overlong '/' sequence
        '_',  'm', 'i', 'd', 'd', 'l', 'e',  '_',
        0xFF, // Invalid byte
        '_',  'e', 'n', 'd'};
    std::u8string invalid_u8(invalid_u8_data,
                             sizeof(invalid_u8_data) / sizeof(char8_t));

    SUBCASE("UseReplacementCharacter") {
      auto res = wutils::u32s(invalid_u8,
                              wutils::ErrorPolicy::UseReplacementCharacter);
      CHECK_FALSE(res.is_valid);
      const std::u32string expected =
          U"start_"s + wutils::detail::REPLACEMENT_CHAR_32 +
          wutils::detail::REPLACEMENT_CHAR_32 + U"_middle_" +
          wutils::detail::REPLACEMENT_CHAR_32 + U"_end";
      CHECK(res.value == expected);
    }

    SUBCASE("SkipInvalidValues") {
      auto res =
          wutils::u32s(invalid_u8, wutils::ErrorPolicy::SkipInvalidValues);
      CHECK_FALSE(res.is_valid);
      CHECK(res.value == U"start__middle__end");
    }

    SUBCASE("StopOnFirstError") {
      auto res =
          wutils::u32s(invalid_u8, wutils::ErrorPolicy::StopOnFirstError);
      CHECK_FALSE(res.is_valid);
      CHECK(res.value == U"start_");
    }
  }

  SUBCASE("Invalid UTF-16 sequences") {
    const char16_t invalid_u16_data[] = {u's',   u't', u'a', u'r', u't', u'_',
                                         0xD800, // Unpaired high surrogate
                                         u'_',   u'm', u'i', u'd', u'd', u'l',
                                         u'e',   u'_',
                                         0xDFFF, // Unpaired low surrogate
                                         u'_',   u'e', 'n',  'd'};
    std::u16string invalid_u16(invalid_u16_data,
                               sizeof(invalid_u16_data) / sizeof(char16_t));

    SUBCASE("UseReplacementCharacter") {
      auto res = wutils::u8s(invalid_u16,
                             wutils::ErrorPolicy::UseReplacementCharacter);
      CHECK_FALSE(res.is_valid);
      const std::u8string expected =
          u8"start_"s + std::u8string(wutils::detail::REPLACEMENT_CHAR_8) +
          u8"_middle_" + std::u8string(wutils::detail::REPLACEMENT_CHAR_8) +
          u8"_end";
      CHECK(res.value == expected);
    }

    SUBCASE("SkipInvalidValues") {
      auto res =
          wutils::u8s(invalid_u16, wutils::ErrorPolicy::SkipInvalidValues);
      CHECK_FALSE(res.is_valid);
      CHECK(res.value == u8"start__middle__end");
    }

    SUBCASE("StopOnFirstError") {
      auto res =
          wutils::u8s(invalid_u16, wutils::ErrorPolicy::StopOnFirstError);
      CHECK_FALSE(res.is_valid);
      CHECK(res.value == u8"start_");
    }
  }
}

TEST_CASE("Width edge cases") {
  SUBCASE("Embedded NUL does not truncate the width scan") {
    // std::u32string is length-prefixed and may legally contain an embedded
    // NUL codepoint; the scan must be bounded by length, not by NUL.
    const char32_t data[] = {U'a', U'b', 0, U'c', U'd'};
    std::u32string s(data, sizeof(data) / sizeof(char32_t));
    CHECK(wutils::uswidth(s) == 4);
  }

  SUBCASE("Combining/format characters above U+200D are zero-width") {
    // These codepoints previously fell past the (broken) bisearch table's
    // upper-bound guard and were misreported as width 1 or 2.
    CHECK(wutils::uswidth(std::u32string(1, 0x20D0)) ==
          0); // COMBINING LEFT HARPOON ABOVE
    CHECK(wutils::uswidth(std::u32string(1, 0xFEFF)) ==
          0); // ZERO WIDTH NO-BREAK SPACE
    CHECK(wutils::uswidth(std::u32string(1, 0xFE0F)) ==
          0); // VARIATION SELECTOR-16
    CHECK(wutils::uswidth(std::u32string(1, 0x1F3FB)) ==
          0); // standalone skin tone modifier

    std::u32string e_with_combining = U"e";
    e_with_combining.push_back(0x20D0);
    CHECK(wutils::uswidth(e_with_combining) == 1);

    std::u32string digit_with_vs16 = U"1";
    digit_with_vs16.push_back(0xFE0F);
    CHECK(wutils::uswidth(digit_with_vs16) == 1);
  }
}
