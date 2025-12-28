
// Original Source: https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
// Changes:
// - Port to C++
// - Use fixed-length char and string types instead of wchar_t
// - Added a method for UTF-16 to UTF-32 conversion

/*
 * This is an implementation of wcwidth() and wcswidth() (defined in
 * IEEE Std 1002.1-2001) for Unicode.
 *
 * http://www.opengroup.org/onlinepubs/007904975/functions/wcwidth.html
 * http://www.opengroup.org/onlinepubs/007904975/functions/wcswidth.html
 *
 * In fixed-width output devices, Latin characters all occupy a single
 * "cell" position of equal width, whereas ideographic CJK characters
 * occupy two such cells. Interoperability between terminal-line
 * applications and (teletype-style) character terminals using the
 * UTF-8 encoding requires agreement on which character should advance
 * the cursor by how many cell positions. No established formal
 * standards exist at present on which Unicode character shall occupy
 * how many cell positions on character terminals. These routines are
 * a first attempt of defining such behavior based on simple rules
 * applied to data provided by the Unicode Consortium.
 *
 * For some graphical characters, the Unicode standard explicitly
 * defines a character-cell width via the definition of the East Asian
 * FullWidth (F), Wide (W), Half-width (H), and Narrow (Na) classes.
 * In all these cases, there is no ambiguity about which width a
 * terminal shall use. For characters in the East Asian Ambiguous (A)
 * class, the width choice depends purely on a preference of backward
 * compatibility with either historic CJK or Western practice.
 * Choosing single-width for these characters is easy to justify as
 * the appropriate long-term solution, as the CJK practice of
 * displaying these characters as double-width comes from historic
 * implementation simplicity (8-bit encoded characters were displayed
 * single-width and 16-bit ones double-width, even for Greek,
 * Cyrillic, etc.) and not any typographic considerations.
 *
 * Much less clear is the choice of width for the Not East Asian
 * (Neutral) class. Existing practice does not dictate a width for any
 * of these characters. It would nevertheless make sense
 * typographically to allocate two character cells to characters such
 * as for instance EM SPACE or VOLUME INTEGRAL, which cannot be
 * represented adequately with a single-width glyph. The following
 * routines at present merely assign a single-cell width to all
 * neutral characters, in the interest of simplicity. This is not
 * entirely satisfactory and should be reconsidered before
 * establishing a formal standard in this area. At the moment, the
 * decision which Not East Asian (Neutral) characters should be
 * represented by double-width glyphs cannot yet be answered by
 * applying a simple rule from the Unicode database content. Setting
 * up a proper standard for the behavior of UTF-8 character terminals
 * will require a careful analysis not only of each Unicode character,
 * but also of each presentation form, something the author of these
 * routines has avoided to do so far.
 *
 * http://www.unicode.org/unicode/reports/tr11/
 *
 * Markus Kuhn -- 2007-05-26 (Unicode 5.0)
 *
 * Permission to use, copy, modify, and distribute this software
 * for any purpose and without fee is hereby granted. The author
 * disclaims all warranties with regard to this software.
 *
 * Latest version: http://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
 */
#include <cstddef>

#include <stdint.h>
#include <uchar.h>
#include <wchar.h>

#include <string>
#include <string_view>

#include "wutils_internal.hpp"
#ifdef _WIN32
#include <Windows.h>
#endif

using std::size_t;

namespace internal {

struct interval {
  char32_t first;
  char32_t last;
};

/* auxiliary function for binary search in interval table */
static bool bisearch(char32_t ucs, const struct interval *table, char32_t max) {
  char32_t min = 0;
  char32_t mid;

  if (ucs < table[0].first || ucs > table[max].last)
    return false;
  while (max >= min) {
    mid = (min + max) / 2;
    if (ucs > table[mid].last)
      min = mid + 1;
    else if (ucs < table[mid].first)
      max = mid - 1;
    else
      return true;
  }

  return false;
}

/* The following two functions define the column width of an ISO 10646
 * character as follows:
 *
 *    - The null character (U+0000) has a column width of 0.
 *
 *    - Other C0/C1 control characters and DEL will lead to a return
 *      value of -1.
 *
 *    - Non-spacing and enclosing combining characters (general
 *      category code Mn or Me in the Unicode database) have a
 *      column width of 0.
 *
 *    - SOFT HYPHEN (U+00AD) has a column width of 1.
 *
 *    - Other format characters (general category code Cf in the Unicode
 *      database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
 *
 *    - Hangul Jamo medial vowels and final consonants (U+1160-U+11FF)
 *      have a column width of 0.
 *
 *    - Spacing characters in the East Asian Wide (W) or East Asian
 *      Full-width (F) category as defined in Unicode Technical
 *      Report #11 have a column width of 2.
 *
 *    - All remaining characters (including all printable
 *      ISO 8859-1 and WGL4 characters, Unicode control characters,
 *      etc.) have a column width of 1.
 *
 * This implementation assumes that wchar_t characters are encoded
 * in ISO 10646.
 */

int mk_wcwidth(char32_t ucs) {
  /* sorted list of non-overlapping intervals of non-spacing characters */
  /* generated by "uniset +cat=Me +cat=Mn +cat=Cf -00AD +1160-11FF +200B c" */
  static const struct interval combining[] = {
      {0x0300, 0x036F},
      {0x0483, 0x0486},
      {0x0488, 0x0489},
      {0x0591, 0x05BD},
      {0x05BF, 0x05BF},
      {0x05C1, 0x05C2},
      {0x05C4, 0x05C5},
      {0x05C7, 0x05C7},
      {0x0600, 0x0603},
      {0x0610, 0x0615},
      {0x064B, 0x065E},
      {0x0670, 0x0670},
      {0x06D6, 0x06E4},
      {0x06E7, 0x06E8},
      {0x06EA, 0x06ED},
      {0x070F, 0x070F},
      {0x0711, 0x0711},
      {0x0730, 0x074A},
      {0x07A6, 0x07B0},
      {0x07EB, 0x07F3},
      {0x0901, 0x0902},
      {0x093C, 0x093C},
      {0x0941, 0x0948},
      {0x094D, 0x094D},
      {0x0951, 0x0954},
      {0x0962, 0x0963},
      {0x0981, 0x0981},
      {0x09BC, 0x09BC},
      {0x09C1, 0x09C4},
      {0x09CD, 0x09CD},
      {0x09E2, 0x09E3},
      {0x0A01, 0x0A02},
      {0x0A3C, 0x0A3C},
      {0x0A41, 0x0A42},
      {0x0A47, 0x0A48},
      {0x0A4B, 0x0A4D},
      {0x0A70, 0x0A71},
      {0x0A81, 0x0A82},
      {0x0ABC, 0x0ABC},
      {0x0AC1, 0x0AC5},
      {0x0AC7, 0x0AC8},
      {0x0ACD, 0x0ACD},
      {0x0AE2, 0x0AE3},
      {0x0B01, 0x0B01},
      {0x0B3C, 0x0B3C},
      {0x0B3F, 0x0B3F},
      {0x0B41, 0x0B43},
      {0x0B4D, 0x0B4D},
      {0x0B56, 0x0B56},
      {0x0B82, 0x0B82},
      {0x0BC0, 0x0BC0},
      {0x0BCD, 0x0BCD},
      {0x0C3E, 0x0C40},
      {0x0C46, 0x0C48},
      {0x0C4A, 0x0C4D},
      {0x0C55, 0x0C56},
      {0x0CBC, 0x0CBC},
      {0x0CBF, 0x0CBF},
      {0x0CC6, 0x0CC6},
      {0x0CCC, 0x0CCD},
      {0x0CE2, 0x0CE3},
      {0x0D41, 0x0D43},
      {0x0D4D, 0x0D4D},
      {0x0DCA, 0x0DCA},
      {0x0DD2, 0x0DD4},
      {0x0DD6, 0x0DD6},
      {0x0E31, 0x0E31},
      {0x0E34, 0x0E3A},
      {0x0E47, 0x0E4E},
      {0x0EB1, 0x0EB1},
      {0x0EB4, 0x0EB9},
      {0x0EBB, 0x0EBC},
      {0x0EC8, 0x0ECD},
      {0x0F18, 0x0F19},
      {0x0F35, 0x0F35},
      {0x0F37, 0x0F37},
      {0x0F39, 0x0F39},
      {0x0F71, 0x0F7E},
      {0x0F80, 0x0F84},
      {0x0F86, 0x0F87},
      {0x0F90, 0x0F97},
      {0x0F99, 0x0FBC},
      {0x0FC6, 0x0FC6},
      {0x102D, 0x1030},
      {0x1032, 0x1032},
      {0x1036, 0x1037},
      {0x1039, 0x1039},
      {0x1058, 0x1059},
      {0x1160, 0x11FF},
      {0x135F, 0x135F},
      {0x1712, 0x1714},
      {0x1732, 0x1734},
      {0x1752, 0x1753},
      {0x1772, 0x1773},
      {0x17B4, 0x17B5},
      {0x17B7, 0x17BD},
      {0x17C6, 0x17C6},
      {0x17C9, 0x17D3},
      {0x17DD, 0x17DD},
      {0x180B, 0x180D},
      {0x18A9, 0x18A9},
      {0x1920, 0x1922},
      {0x1927, 0x1928},
      {0x1932, 0x1932},
      {0x1939, 0x193B},
      {0x1A17, 0x1A18},
      {0x1B00, 0x1B03},
      {0x1B34, 0x1B34},
      {0x1B36, 0x1B3A},
      {0x1B3C, 0x1B3C},
      {0x1B42, 0x1B42},
      {0x1B6B, 0x1B73},
      {0x1DC0, 0x1DCA},
      {0x1DFE, 0x1DFF},
      {0x200B, 0x200F},
      {0x202A, 0x202E},
      {0x2060, 0x2063},
      {0x206A, 0x206F},
      {0x20D0, 0x20EF},
      {0x302A, 0x302F},
      {0x3099, 0x309A},
      {0xA806, 0xA806},
      {0xA80B, 0xA80B},
      {0xA825, 0xA826},
      {0xFB1E, 0xFB1E},
      {0xFE00, 0xFE0F},
      {0xFE20, 0xFE23},
      {0xFEFF, 0xFEFF},
      {0xFFF9, 0xFFFB},
      {0x10A01, 0x10A03},
      {0x10A05, 0x10A06},
      {0x10A0C, 0x10A0F},
      {0x10A38, 0x10A3A},
      {0x10A3F, 0x10A3F},
      {0x1D167, 0x1D169},
      {0x1D173, 0x1D182},
      {0x1D185, 0x1D18B},
      {0x1D1AA, 0x1D1AD},
      {0x1D242, 0x1D244},
      {0xE0001, 0xE0001},
      {0xE0020, 0xE007F},
      {0xE0100, 0xE01EF},
      /* Add emoji modifiers */
      {0x1F3FB, 0x1F3FF}, /* Emoji skin tone modifiers */
      {0x200D, 0x200D} /* Zero Width Joiner - explicitly listed for clarity */
  };

  /* test for 8-bit control characters */
  if (ucs == 0)
    return 0;
  if (ucs < 32 || (ucs >= 0x7f && ucs < 0xa0))
    return -1;

  /* binary search in table of non-spacing characters */
  if (bisearch(ucs, combining, sizeof(combining) / sizeof(struct interval) - 1))
    return 0;

  /* if we arrive here, ucs is not a combining or C0/C1 control character */

  return 1 +
         ((ucs >= 0x1100 && ucs <= 0x115f) || /* Hangul Jamo init. consonants */
          ucs == 0x2329 || ucs == 0x232a ||
          (ucs >= 0x2e80 && ucs <= 0xa4cf && ucs != 0x303f) || /* CJK ... Yi */
          (ucs >= 0xac00 && ucs <= 0xd7a3) || /* Hangul Syllables */
          (ucs >= 0xf900 && ucs <= 0xfaff) || /* CJK Compatibility Ideographs */
          (ucs >= 0xfe10 && ucs <= 0xfe19) || /* Vertical forms */
          (ucs >= 0xfe30 && ucs <= 0xfe6f) || /* CJK Compatibility Forms */
          (ucs >= 0xff00 && ucs <= 0xff60) || /* Fullwidth Forms */
          (ucs >= 0xffe0 && ucs <= 0xffe6) ||
          (ucs >= 0x20000 && ucs <= 0x2fffd) ||
          (ucs >= 0x30000 && ucs <= 0x3fffd) ||
          /* Emoji and symbol ranges (updated for latest Unicode) */
          (ucs >= 0x1F000 && ucs <= 0x1F9FF) || /* Emoji and various symbols */
          (ucs >= 0x1FA00 && ucs <= 0x1FA6F) || /* Chess symbols and others */
          (ucs >= 0x1FA70 &&
           ucs <= 0x1FAFF)); /* Symbols and Pictographs Extended-A */
}

/* This function properly handles complex emoji sequences */
int mk_wcswidth(const char32_t *pwcs, size_t n) {
  int width = 0;
  const char32_t *p = pwcs;
  size_t remaining = n;

  while (*p && remaining > 0) {
    char32_t base_char = *p;
    int char_width = mk_wcwidth(base_char);

    if (char_width < 0)
      return -1;

    // Check if this is the start of an emoji sequence
    bool is_emoji = (base_char >= 0x1F000 && base_char <= 0x1FAFF) ||
                    (base_char >= 0x2600 && base_char <= 0x27BF);

    if (is_emoji) {
      // Add the base emoji width
      width += char_width;
      p++;
      remaining--;

      // Skip any subsequent ZWJ sequences, skin tone modifiers, etc.
      while (remaining > 0 && *p &&
             (((*p >= 0x1F3FB && *p <= 0x1F3FF) ||   // Skin tone modifiers
               (*p == 0x200D) ||                     // Zero Width Joiner
               (*p == 0xFE0F) ||                     // Variation Selector-16
               (*p >= 0xE0020 && *p <= 0xE007F)))) { // Tag sequences

        // If we hit a ZWJ followed by another emoji, don't count the
        // joined emoji's width
        if (*p == 0x200D && remaining > 1 && *(p + 1)) {
          p++; // Skip the ZWJ
          remaining--;

          // Skip the next emoji too (but don't add its width)
          if (remaining > 0 && *p) {
            // Check if it's an emoji
            if ((*p >= 0x1F000 && *p <= 0x1FAFF) ||
                (*p >= 0x2600 && *p <= 0x27BF)) {
              p++;
              remaining--;
            }
          }
        } else {
          // Skip other combining characters
          p++;
          remaining--;
        }
      }
    } else {
      // Regular character
      width += char_width;
      p++;
      remaining--;
    }
  }

  return width;
}

} // namespace internal

int wutils::uswidth(const std::u32string_view u32s) {
  return internal::mk_wcswidth(u32s.data(), u32s.size());
}

int wutils::uswidth(const std::u16string_view u16s) {
  wutils::ConversionResult<std::u32string> u32s =
      wutils::detail::u32(u16s, wutils::ErrorPolicy::SkipInvalidValues);
  return internal::mk_wcswidth(u32s->data(), u32s->size());
}

int wutils::uswidth(const std::u8string_view u8s) {
  wutils::ConversionResult<std::u32string> u32s =
      wutils::detail::u32(u8s, wutils::ErrorPolicy::SkipInvalidValues);
  return internal::mk_wcswidth(u32s->data(), u32s->size());
}

#ifdef _WIN32
void wutils::wcout(const std::wstring_view ws) {
  WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE), ws.data(),
                static_cast<DWORD>(ws.size()), NULL, NULL);
}
void wutils::wcerr(const std::wstring_view ws) {
  WriteConsoleW(GetStdHandle(STD_ERROR_HANDLE), ws.data(),
                static_cast<DWORD>(ws.size()), NULL, NULL);
}
#endif

/* UTF conversion */

namespace internal {
// A struct to hold the result of a single decoding operation
struct DecodeResult {
  char32_t codepoint;    // The decoded Unicode codepoint
  size_t consumed_units; // Number of input units (e.g., char8_t) consumed
  bool is_valid;         // Was the sequence valid?
};

// Decodes one character from a UTF-8 stream, with validation.
DecodeResult decode_one_utf8(std::u8string_view input) {
  if (input.empty()) {
    return {0, 0, false};
  }

  unsigned char c = input[0];

  if (c < 0x80) { // 0xxxxxxx
    return {c, 1, true};
  }

  if (c < 0xC2) {
    return {0, 1, false};
  } // Invalid sequence

  if (c < 0xE0) { // 110xxxxx 10xxxxxx
    if (input.size() < 2 || (input[1] & 0xC0) != 0x80) {
      return {0, 1, false};
    }
    char32_t cp = ((c & 0x1F) << 6) | (input[1] & 0x3F);
    if (cp < 0x80) {
      return {0, 1, false};
    } // Overlong
    return {cp, 2, true};
  }

  if (c < 0xF0) { // 1110xxxx 10xxxxxx 10xxxxxx
    if (input.size() < 3 || (input[1] & 0xC0) != 0x80 ||
        (input[2] & 0xC0) != 0x80) {
      return {0, 1, false};
    }
    char32_t cp =
        ((c & 0x0F) << 12) | ((input[1] & 0x3F) << 6) | (input[2] & 0x3F);
    if (cp < 0x800) {
      return {0, 1, false};
    } // Overlong
    if (cp >= 0xD800 && cp <= 0xDFFF) {
      return {0, 1, false};
    } // Surrogates not allowed
    return {cp, 3, true};
  }

  if (c < 0xF5) { // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    if (input.size() < 4 || (input[1] & 0xC0) != 0x80 ||
        (input[2] & 0xC0) != 0x80 || (input[3] & 0xC0) != 0x80) {
      return {0, 1, false};
    }
    char32_t cp = ((c & 0x07) << 18) | ((input[1] & 0x3F) << 12) |
                  ((input[2] & 0x3F) << 6) | (input[3] & 0x3F);
    if (cp < 0x10000) {
      return {0, 1, false};
    } // Overlong
    if (cp > 0x10FFFF) {
      return {0, 1, false};
    } // Out of range
    return {cp, 4, true};
  }

  return {0, 1, false}; // Invalid sequence
}

// Decodes one character from a UTF-16 stream, with validation.
DecodeResult decode_one_utf16(std::u16string_view input) {
  if (input.empty()) {
    return {0, 0, false};
  }
  char16_t c1 = input[0];
  if (c1 < 0xD800 || c1 > 0xDFFF) {
    return {c1, 1, true}; // Not a surrogate
  }
  if (c1 > 0xDBFF || input.size() < 2) {
    return {0, 1, false}; // Lone low surrogate or truncated sequence
  }
  char16_t c2 = input[1];
  if (c2 < 0xDC00 || c2 > 0xDFFF) {
    return {0, 1, false}; // High surrogate not followed by low surrogate
  }
  char32_t codepoint = 0x10000 + (((c1 - 0xD800) << 10) | (c2 - 0xDC00));
  return {codepoint, 2, true};
}

// Encodes a codepoint and appends it to a UTF-8 string.
void encode_utf8(char32_t codepoint, std::u8string &output) {
  if (codepoint <= 0x7F) {
    output.push_back(static_cast<char8_t>(codepoint));
  } else if (codepoint <= 0x7FF) {
    output.push_back(static_cast<char8_t>(0xC0 | (codepoint >> 6)));
    output.push_back(static_cast<char8_t>(0x80 | (codepoint & 0x3F)));
  } else if (codepoint <= 0xFFFF) {
    output.push_back(static_cast<char8_t>(0xE0 | (codepoint >> 12)));
    output.push_back(static_cast<char8_t>(0x80 | ((codepoint >> 6) & 0x3F)));
    output.push_back(static_cast<char8_t>(0x80 | (codepoint & 0x3F)));
  } else {
    output.push_back(static_cast<char8_t>(0xF0 | (codepoint >> 18)));
    output.push_back(static_cast<char8_t>(0x80 | ((codepoint >> 12) & 0x3F)));
    output.push_back(static_cast<char8_t>(0x80 | ((codepoint >> 6) & 0x3F)));
    output.push_back(static_cast<char8_t>(0x80 | (codepoint & 0x3F)));
  }
}

// Encodes a codepoint and appends it to a UTF-16 string.
void encode_utf16(char32_t codepoint, std::u16string &output) {
  if (codepoint <= 0xFFFF) {
    output.push_back(static_cast<char16_t>(codepoint));
  } else {
    char16_t high = static_cast<char16_t>(0xD800) +
                    static_cast<char16_t>((codepoint - 0x10000) >> 10);
    char16_t low = static_cast<char16_t>(0xDC00) +
                   static_cast<char16_t>((codepoint - 0x10000) & 0x3FF);
    output.push_back(high);
    output.push_back(low);
  }
}
} // namespace internal

// UTF-16 to UTF-8 conversion
wutils::ConversionResult<std::u8string>
wutils::detail::u8(const std::u16string_view u16s,
                   const ErrorPolicy errorPolicy) {
  bool is_valid = true;
  std::u8string result;
  result.reserve(u16s.size());

  for (size_t i = 0; i < u16s.size();) {
    internal::DecodeResult decoded = internal::decode_one_utf16(u16s.substr(i));
    if (decoded.is_valid) {
      internal::encode_utf8(decoded.codepoint, result);
    } else {
      is_valid = false;
      switch (errorPolicy) {
      case ErrorPolicy::SkipInvalidValues:
        break;
      case ErrorPolicy::StopOnFirstError:
        return {result, false};
      case ErrorPolicy::UseReplacementCharacter:
        result.append(wutils::detail::REPLACEMENT_CHAR_8);
        break;
      }
    }
    i += decoded.consumed_units;
  }
  return {result, is_valid};
}

// UTF-32 to UTF-8 conversion
wutils::ConversionResult<std::u8string>
wutils::detail::u8(const std::u32string_view u32s,
                   const ErrorPolicy errorPolicy) {
  bool is_valid = true;
  std::u8string result;
  result.reserve(u32s.size());

  for (char32_t codepoint : u32s) {
    if (codepoint <= 0x10FFFF &&
        !(codepoint >= 0xD800 && codepoint <= 0xDFFF)) {
      internal::encode_utf8(codepoint, result);
    } else {
      is_valid = false;
      switch (errorPolicy) {
      case ErrorPolicy::SkipInvalidValues:
        continue;
      case ErrorPolicy::StopOnFirstError:
        return {result, false};
      case ErrorPolicy::UseReplacementCharacter:
        result.append(wutils::detail::REPLACEMENT_CHAR_8);
        break;
      }
    }
  }
  return {result, is_valid};
}

// UTF-8 to UTF-16 conversion
wutils::ConversionResult<std::u16string>
wutils::detail::u16(const std::u8string_view u8s,
                    const ErrorPolicy errorPolicy) {
  bool is_valid = true;
  std::u16string result;
  result.reserve(u8s.size());

  for (size_t i = 0; i < u8s.size();) {
    internal::DecodeResult decoded = internal::decode_one_utf8(u8s.substr(i));
    if (decoded.is_valid) {
      internal::encode_utf16(decoded.codepoint, result);
    } else {
      is_valid = false;
      switch (errorPolicy) {
      case ErrorPolicy::SkipInvalidValues:
        break;
      case ErrorPolicy::StopOnFirstError:
        return {result, false};
      case ErrorPolicy::UseReplacementCharacter:
        internal::encode_utf16(wutils::detail::REPLACEMENT_CHAR_32, result);
        break;
      }
    }
    i += decoded.consumed_units;
  }
  return {result, is_valid};
}

// UTF-32 to UTF-16 conversion
wutils::ConversionResult<std::u16string>
wutils::detail::u16(const std::u32string_view u32s,
                    const ErrorPolicy errorPolicy) {
  bool is_valid = true;
  std::u16string result;
  result.reserve(u32s.size());

  for (char32_t codepoint : u32s) {
    if (codepoint <= 0x10FFFF &&
        !(codepoint >= 0xD800 && codepoint <= 0xDFFF)) {
      internal::encode_utf16(codepoint, result);
    } else {
      is_valid = false;
      switch (errorPolicy) {
      case ErrorPolicy::SkipInvalidValues:
        continue;
      case ErrorPolicy::StopOnFirstError:
        return {result, false};
      case ErrorPolicy::UseReplacementCharacter:
        result.push_back(wutils::detail::REPLACEMENT_CHAR_16);
        break;
      }
    }
  }
  return {result, is_valid};
}

// UTF-8 to UTF-32 conversion
wutils::ConversionResult<std::u32string>
wutils::detail::u32(const std::u8string_view u8s,
                    const ErrorPolicy errorPolicy) {
  bool is_valid = true;
  std::u32string result;
  result.reserve(u8s.size());

  for (size_t i = 0; i < u8s.size();) {
    internal::DecodeResult decoded = internal::decode_one_utf8(u8s.substr(i));
    if (decoded.is_valid) {
      result.push_back(decoded.codepoint);
    } else {
      is_valid = false;
      switch (errorPolicy) {
      case ErrorPolicy::SkipInvalidValues:
        break;
      case ErrorPolicy::StopOnFirstError:
        return {result, false};
      case ErrorPolicy::UseReplacementCharacter:
        result.push_back(wutils::detail::REPLACEMENT_CHAR_32);
        break;
      }
    }
    i += decoded.consumed_units;
  }
  return {result, is_valid};
}

// UTF-16 to UTF-32 conversion
wutils::ConversionResult<std::u32string>
wutils::detail::u32(const std::u16string_view u16s,
                    const ErrorPolicy errorPolicy) {
  bool is_valid = true;
  std::u32string result;
  result.reserve(u16s.size());

  for (size_t i = 0; i < u16s.size();) {
    internal::DecodeResult decoded = internal::decode_one_utf16(u16s.substr(i));
    if (decoded.is_valid) {
      result.push_back(decoded.codepoint);
    } else {
      is_valid = false;
      switch (errorPolicy) {
      case ErrorPolicy::SkipInvalidValues:
        break;
      case ErrorPolicy::StopOnFirstError:
        return {result, false};
      case ErrorPolicy::UseReplacementCharacter:
        result.push_back(wutils::detail::REPLACEMENT_CHAR_32);
        break;
      }
    }
    i += decoded.consumed_units;
  }
  return {result, is_valid};
}
