#include <iostream>
#include <string>
#include <string_view>

namespace wutils {
// Windows sucks and can't properly print std::wcout to terminal so we use a
// wrapper
#ifdef _WIN32
void wcout(const std::wstring_view ws);
void wcerr(const std::wstring_view ws);
#else
inline void wcout(const std::wstring_view ws) { std::wcout << ws; }
inline void wcerr(const std::wstring_view ws) { std::wcerr << ws << std::endl; }
#endif

inline void wprint(const std::wstring_view ws) { wcout(ws); }
inline void wprintln(const std::wstring_view ws) {
  wcout(ws);
  wcout(L"\n");
}

// Determines course of action when encountered with an invalid sequence
enum class ErrorPolicy {
  UseReplacementCharacter, // Insert replacement character '�' on error
  SkipInvalidValues, // Skip invalid values and continue conversion to the best
                     // of its ability
  StopOnFirstError   // Stop conversion on the first invalid value, return
                     // partial conversion
};
template <typename T> struct ConversionResult {
  T value;
  bool is_valid;

  T &operator*() { return value; }
  T *operator->() { return &value; }
  const T *operator->() const { return &value; }
  explicit operator bool() const { return is_valid; }
};
namespace detail {

constexpr inline const char8_t *REPLACEMENT_CHAR_8 = u8"�";
constexpr inline const char16_t REPLACEMENT_CHAR_16 = u'�';
constexpr inline const char32_t REPLACEMENT_CHAR_32 = U'�';

// ===== Specialized Conversions =====
inline ConversionResult<std::u8string>
u8(const std::u8string_view u8s,
   [[maybe_unused]] const ErrorPolicy errorPolicy =
       ErrorPolicy::UseReplacementCharacter) {
  return {std::u8string(u8s), true};
}
ConversionResult<std::u8string>
u8(const std::u16string_view u16s,
   const ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter);
ConversionResult<std::u8string>
u8(const std::u32string_view u32s,
   const ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter);

ConversionResult<std::u16string>
u16(const std::u8string_view u8s,
    const ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter);
inline ConversionResult<std::u16string>
u16(const std::u16string_view u16s,
    [[maybe_unused]] const ErrorPolicy errorPolicy =
        ErrorPolicy::UseReplacementCharacter) {
  return {std::u16string(u16s), true};
}
ConversionResult<std::u16string>
u16(const std::u32string_view u32s,
    const ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter);

ConversionResult<std::u32string>
u32(const std::u8string_view u8s,
    const ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter);
ConversionResult<std::u32string>
u32(const std::u16string_view u16s,
    const ErrorPolicy errorPolicy = ErrorPolicy::UseReplacementCharacter);
inline ConversionResult<std::u32string>
u32(const std::u32string_view u32s,
    [[maybe_unused]] const ErrorPolicy errorPolicy =
        ErrorPolicy::UseReplacementCharacter) {
  return {std::u32string(u32s), true};
}
} // namespace detail
int uswidth(const std::u8string_view u8s);
int uswidth(const std::u16string_view u16s);
int uswidth(const std::u32string_view u32s);
} // namespace wutils
