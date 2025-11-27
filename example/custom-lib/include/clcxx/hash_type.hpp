#pragma once

#include <string_view>
#include <type_traits>

namespace clcxx {

using SizeT = std::uint_fast32_t;

// C++ type name from:
// https://stackoverflow.com/questions/81870/is-it-possible-to-prSize-a-variables-type-in-standard-c/56766138#56766138
template <typename T>
constexpr auto TypeName() {
  std::string_view name, prefix, suffix;
#ifdef __clang__
  name = __PRETTY_FUNCTION__;
  prefix = "auto utils::TypeName() [T = ";
  suffix = "]";
#elif defined(__GNUC__)
  name = __PRETTY_FUNCTION__;
  prefix = "constexpr auto utils::TypeName() [with T = ";
  suffix = "]";
#elif defined(_MSC_VER)
  name = __FUNCSIG__;
  prefix = "auto __cdecl utils::TypeName<";
  suffix = ">(void)";
#endif
  name.remove_prefix(prefix.size());
  name.remove_suffix(suffix.size());
  return name;
}

// compile time string hash for types from:
// https://roartindon.blogspot.com/2014/10/compile-time-murmur-hash-in-c.html
namespace detail {
template <SizeT...>
struct Sequence {};
template <SizeT N, SizeT... S>
struct CreateSequence : CreateSequence<N - 1, N - 1, S...> {};
template <SizeT... S>
struct CreateSequence<0, S...> {
  typedef Sequence<S...> Type;
};

constexpr SizeT UpdateHash1(SizeT k) { return k * 0xcc9e2d51; }
constexpr SizeT UpdateHash2(SizeT k) { return (k << 15) | (k >> (32 - 15)); }
constexpr SizeT UpdateHash3(SizeT k) { return k * 0x1b873593; }
constexpr SizeT UpdateHash4(SizeT hash, SizeT block) { return hash ^ block; }
constexpr SizeT UpdateHash5(SizeT hash) {
  return ((hash << 13) | (hash >> (32 - 13))) * 5 + 0xe6546b64;
}

constexpr SizeT UpdateHash(SizeT hash, SizeT block) {
  return UpdateHash5(
      UpdateHash4(hash, UpdateHash3(UpdateHash2(UpdateHash1(block)))));
}

constexpr SizeT UpdateLastHash(SizeT hash, SizeT block) {
  return UpdateHash4(hash, UpdateHash3(UpdateHash2(UpdateHash1(block))));
}

template <typename... C>
constexpr SizeT CalculateHashRounds(SizeT seed, C... c);

template <>
constexpr SizeT CalculateHashRounds(SizeT seed) {
  return seed;
}

template <>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0) {
  return UpdateLastHash(seed, std::uint8_t(c0));
}

template <>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0, char c1) {
  return UpdateLastHash(seed, std::uint8_t(c0) | std::uint8_t(c1) << 8);
}

template <>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0, char c1, char c2) {
  return UpdateLastHash(
      seed, std::uint8_t(c0) | std::uint8_t(c1) << 8 | std::uint8_t(c2) << 16);
}

template <typename... C>
constexpr SizeT CalculateHashRounds(SizeT seed, char c0, char c1, char c2,
                                    char c3, C... c) {
  return CalculateHashRounds(
      UpdateHash(seed, std::uint8_t(c0) | std::uint8_t(c1) << 8 |
                           std::uint8_t(c2) << 16 | std::uint8_t(c3) << 24),
      c...);
}

constexpr SizeT CalculateFinalHash1(SizeT h, SizeT length) {
  return h ^ length;
}

constexpr SizeT CalculateFinalHash2(SizeT h) { return h ^ (h >> 16); }

constexpr SizeT CalculateFinalHash3(SizeT h) { return h * 0x85ebca6b; }

constexpr SizeT CalculateFinalHash4(SizeT h) { return h ^ (h >> 13); }

constexpr SizeT CalculateFinalHash5(SizeT h) { return h * 0xc2b2ae35; }

constexpr SizeT CalculateFinalHash6(SizeT h) { return h ^ (h >> 16); }

constexpr SizeT CalculateFinalHash(SizeT h, SizeT length) {
  return CalculateFinalHash6(
      CalculateFinalHash5(CalculateFinalHash4(CalculateFinalHash3(
          CalculateFinalHash2(CalculateFinalHash1(h, length))))));
}

// This is used to convert from calling const char (&s)[N]
// To CalculateHashRounds(seed, s[0], s[1], s[2], s[3], ... )
template <SizeT N, SizeT... S>
constexpr SizeT Unpack(unsigned seed, const char (&s)[N], Sequence<S...>) {
  return CalculateHashRounds(seed, s[S]...);
}
// This is used to convert from calling std::string_view
// To CalculateHashRounds(seed, s[0], s[1], s[2], s[3], ... )
template <SizeT N, SizeT... S>
constexpr SizeT Unpack(unsigned seed, const std::string_view &s,
                       Sequence<S...>) {
  return CalculateHashRounds(seed, s[S]...);
}

template <SizeT N>
constexpr SizeT murmur3_32(const char (&s)[N], SizeT seed = 0) {
  return CalculateFinalHash(
      Unpack(seed, s, typename CreateSequence<N - 1>::Type()), N - 1);
}

template <SizeT N>
constexpr SizeT murmur3_32(const std::string_view &s, SizeT seed = 0) {
  return CalculateFinalHash(
      Unpack<N>(seed, s, typename CreateSequence<N>::Type()), N);
}

}  // namespace detail

template <typename T>
constexpr auto Hash32TypeName() {
  constexpr auto s = TypeName<T>();
  return detail::murmur3_32<s.length()>(std::move(s));
}

}  // namespace clcxx
