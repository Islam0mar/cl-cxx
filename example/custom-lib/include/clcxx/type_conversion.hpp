#pragma once

#include <complex>
#include <cstdint>
#include <cstring>
#include <memory_resource>
#include <stdexcept>
#include <type_traits>

#include "clcxx_config.hpp"
#include "hash_type.hpp"
#include "memory.hpp"

namespace clcxx {
// supported types are  Primitive CFFI Types with
// void + complex  + struct + class
// POD structs are passed through libffi
// structs,class are passed as pointers with *new*
// (e.g. (:pointer :char))
// (:array :int 10)
// for nonPOD struct, class, pointer
// references are converted to pointers

extern "C" typedef union {
  float Float;
  double Double;
  // long double LongDouble;
} ComplexType;

extern "C" typedef struct {
  ComplexType real;
  ComplexType imag;
} LispComplex;

template <typename T>
inline std::string general_class_name();
template <typename T>
inline std::string pod_class_name();

namespace internal {
template <typename T>
struct is_complex {
  static constexpr bool value = std::is_same_v<T, std::complex<float>> ||
                                std::is_same_v<T, std::complex<double>>;
};

template <typename T>
inline constexpr bool is_complex_v = is_complex<T>::value;

template <typename T>
struct is_pod_struct {
  static constexpr bool value = std::is_trivial_v<T> &&
                                std::is_standard_layout_v<T> &&
                                std::is_class_v<T> && !is_complex_v<T>;
};

template <typename T>
inline constexpr bool is_pod_struct_v = is_pod_struct<T>::value;

template <typename T>
struct is_std_string {
  static constexpr bool value =
      std::is_same_v<std::remove_const_t<T>, std::string>;
};

template <typename T>
inline constexpr bool is_std_string_v = is_std_string<T>::value;

template <typename T>
struct is_general_class {
  static constexpr bool value = !(is_std_string_v<T> || is_complex_v<T> ||
                                  is_pod_struct_v<T>)&&std::is_class_v<T>;
};

template <typename T>
inline constexpr bool is_general_class_v = is_general_class<T>::value;

namespace detail {
template <typename T>
struct unused_type {};

template <typename T1, typename T2>
struct DefineIfDifferent {
  typedef T1 type;
};

template <typename T>
struct DefineIfDifferent<T, T> {
  typedef unused_type<T> type;
};

template <typename T1, typename T2>
using define_if_different = typename DefineIfDifferent<T1, T2>::type;

}  // namespace detail

/// Convenience function to get the lisp data type associated with T
template <typename T>
struct static_type_mapping {
  typedef typename std::conditional_t<is_pod_struct_v<T>, T, void *> type;
  static std::string lisp_type() {
    static_assert(std::is_class_v<T>, "Unkown type");
    using ClassT = std::remove_cv_t<T>;
    if constexpr (is_pod_struct_v<ClassT>)
      return std::string("(:struct " + pod_class_name<ClassT>() + ")");
    else
      return std::string("(:class " + general_class_name<ClassT>() + ")");
  }
};

template <>
struct static_type_mapping<char> {
  typedef char type;
  static std::string lisp_type() { return ":char"; }
};
template <>
struct static_type_mapping<
    detail::define_if_different<unsigned char, uint8_t>> {
  typedef unsigned char type;
  static std::string lisp_type() { return ":uchar"; }
};
template <>
struct static_type_mapping<detail::define_if_different<short, int16_t>> {
  typedef short type;
  static std::string lisp_type() { return ":short"; }
};
template <>
struct static_type_mapping<
    detail::define_if_different<unsigned short, uint16_t>> {
  typedef unsigned short type;
  static std::string lisp_type() { return ":ushort"; }
};
template <>
struct static_type_mapping<detail::define_if_different<int, int32_t>> {
  typedef int type;
  static std::string lisp_type() { return ":int"; }
};
template <>
struct static_type_mapping<
    detail::define_if_different<unsigned int, uint32_t>> {
  typedef unsigned int type;
  static std::string lisp_type() { return ":uint"; }
};
template <>
struct static_type_mapping<detail::define_if_different<long, int64_t>> {
  typedef long type;
  static std::string lisp_type() { return ":long"; }
};
template <>
struct static_type_mapping<
    detail::define_if_different<unsigned long, uint64_t>> {
  typedef unsigned long type;
  static std::string lisp_type() { return ":ulong"; }
};
template <>
struct static_type_mapping<detail::define_if_different<long long, int64_t>> {
  typedef long long type;
  static std::string lisp_type() { return ":llong"; }
};
template <>
struct static_type_mapping<
    detail::define_if_different<unsigned long long, uint64_t>> {
  typedef unsigned long long type;
  static std::string lisp_type() { return ":ullong"; }
};
template <>
struct static_type_mapping<int8_t> {
  typedef int8_t type;
  static std::string lisp_type() { return ":int8"; }
};
template <>
struct static_type_mapping<uint8_t> {
  typedef uint8_t type;
  static std::string lisp_type() { return ":uint8"; }
};
template <>
struct static_type_mapping<int16_t> {
  typedef int16_t type;
  static std::string lisp_type() { return ":int16"; }
};
template <>
struct static_type_mapping<uint16_t> {
  typedef uint16_t type;
  static std::string lisp_type() { return ":uint16"; }
};
template <>
struct static_type_mapping<int32_t> {
  typedef int32_t type;
  static std::string lisp_type() { return ":int32"; }
};
template <>
struct static_type_mapping<uint32_t> {
  typedef uint32_t type;
  static std::string lisp_type() { return ":uint32"; }
};
template <>
struct static_type_mapping<int64_t> {
  typedef int64_t type;
  static std::string lisp_type() { return ":int64"; }
};
template <>
struct static_type_mapping<uint64_t> {
  typedef uint64_t type;
  static std::string lisp_type() { return ":uint64"; }
};
template <>
struct static_type_mapping<float> {
  typedef float type;
  static std::string lisp_type() { return ":float"; }
};
template <>
struct static_type_mapping<double> {
  typedef double type;
  static std::string lisp_type() { return ":double"; }
};
// template <> struct static_type_mapping<long double> {
//   typedef long double type;
//   static std::string lisp_type() { return ":long-double"; }
// };

// References
template <typename T>
struct static_type_mapping<T &> {
  // reference to any type => passed and returned as pointers
  typedef void *type;
  static std::string lisp_type() {
    return std::string("(:reference " + static_type_mapping<T>::lisp_type() +
                       ")");
  }
};

template <typename T>
struct static_type_mapping<const T &> {
  // reference to any type => passed and returned as pointers
  typedef const void *type;
  static std::string lisp_type() {
    return std::string("(:const-reference " +
                       static_type_mapping<T>::lisp_type() + ")");
  }
};

// resolve const types
template <typename T>
struct static_type_mapping<const T> {
  using l_type = typename static_type_mapping<T>::type;
  typedef l_type const type;
  static std::string lisp_type() { return static_type_mapping<T>::lisp_type(); }
};

template <typename T>
struct static_type_mapping<T *> {
  typedef void *type;
  static std::string lisp_type() {
    return std::string("(:pointer " + static_type_mapping<T>::lisp_type() +
                       ")");
  }
};

template <typename T, std::size_t N>
struct static_type_mapping<T (&)[N]> {
  typedef T type[N];
  static std::string lisp_type() {
    return std::string("(:array " + static_type_mapping<T>::lisp_type() + " " +
                       N + ")");
  }
};
template <typename T, std::size_t N>
struct static_type_mapping<T (*)[N]> {
  typedef T type[N];
  static std::string lisp_type() {
    return std::string("(:array " + static_type_mapping<T>::lisp_type() + " " +
                       N + ")");
  }
};

template <>
struct static_type_mapping<bool> {
  typedef bool type;
  static std::string lisp_type() { return ":bool"; }
};
template <>
struct static_type_mapping<const char *> {
  typedef const char *type;
  static std::string lisp_type() { return ":string+ptr"; }
};
template <>
struct static_type_mapping<std::string> {
  typedef const char *type;
  static std::string lisp_type() { return ":string+ptr"; }
};
template <>
struct static_type_mapping<void> {
  typedef void type;
  static std::string lisp_type() { return ":void"; }
};

template <>
struct static_type_mapping<std::complex<float>> {
  typedef LispComplex type;
  static std::string lisp_type() {
    return std::string(std::string("(:complex ") +
                       static_type_mapping<float>::lisp_type() + ")");
  }
};

template <>
struct static_type_mapping<std::complex<double>> {
  typedef LispComplex type;
  static std::string lisp_type() {
    return std::string(std::string("(:complex ") +
                       static_type_mapping<double>::lisp_type() + ")");
  }
};

// ------------------------------------------------------------------//
// Box an automatically converted value
/// Wrap a C++ pointer in void pointer lisp cffi
template <typename CppT, typename LispT>
inline LispT box(CppT cpp_val) {
  static_assert(std::is_pointer_v<CppT>, "Box unkown type");
  return static_cast<LispT>(cpp_val);
}

template <>
inline const char *box(const char *str) {
  const auto n = std::strlen(str);
  auto new_str = static_cast<char *>(
      MemPool().allocate((n + 1) * sizeof(char), std::alignment_of_v<char>));
  std::strcpy(new_str, str);
  new_str[n] = '\0';
  return new_str;
}

template <>
inline LispComplex box(std::complex<float> x) {
  LispComplex c;
  c.real.Float = std::real(x);
  c.imag.Float = std::imag(x);
  return c;
}

template <>
inline LispComplex box(std::complex<double> x) {
  LispComplex c;
  c.real.Double = std::real(x);
  c.imag.Double = std::imag(x);
  return c;
}

// template <> inline LispComplex box(std::complex<long double> x) {
//   LispComplex c;
//   c.real.LongDouble = std::real(x);
//   c.imag.LongDouble = std::imag(x);
//   return c;
// }

// unbox -----------------------------------------------------------------//
/// pointers
template <typename CppT, typename LispT>
CppT unbox(LispT lisp_val) {
  static_assert(std::is_pointer_v<CppT>, "Box unkown type");
  return static_cast<CppT>(lisp_val);
}

template <>
inline const char *unbox(const char *v) {
  return v;
}

template <>
inline std::complex<float> unbox(LispComplex v) {
  return std::complex<float>(v.real.Float, v.imag.Float);
}

template <>
inline std::complex<double> unbox(LispComplex v) {
  return std::complex<double>(v.real.Double, v.imag.Double);
}

// template <> inline std::complex<long double> unbox(LispComplex v) {
//   return std::complex<long double>(v.real.LongDouble, v.imag.LongDouble);
// }

/////////////////////////////////////

// Base template for converting to CPP
template <typename CppT, typename Enable = void>
struct ConvertToCpp {
  using LispT = void;
  template <typename T>
  inline CppT *operator()(T &&) {
    static_assert(sizeof(CppT) == 0,
                  "No appropriate specialization for ConvertToCpp");
    return nullptr;
  }
};

// Fundamental/array/pod types conversion
template <typename CppT>
struct ConvertToCpp<CppT, typename std::enable_if_t<
                              std::is_fundamental_v<CppT> ||
                              std::is_array_v<CppT> || is_pod_struct_v<CppT>>> {
  using LispT = typename static_type_mapping<CppT>::type;
  inline CppT operator()(LispT lisp_val) const {
    static_assert(std::is_same_v<LispT, CppT>, "Fundamental type mismatch");
    return lisp_val;
  }
};

namespace detail {
template <typename CppT, typename LispT>
struct RefToCpp {
  // reference to pointer
  CppT operator()(LispT lisp_val) const {
    auto obj_ptr =
        static_cast<typename std::remove_reference_t<CppT> *>(lisp_val);
    return *obj_ptr;
  }
};
}  // namespace detail

// reference conversion
template <typename CppT>
struct ConvertToCpp<CppT,
                    typename std::enable_if_t<std::is_reference_v<CppT>>> {
  using LispT = typename static_type_mapping<CppT>::type;
  CppT operator()(LispT lisp_val) const {
    static_assert(
        std::is_same_v<LispT, void *> || std::is_same_v<LispT, const void *>,
        "type mismatch");
    return detail::RefToCpp<CppT, LispT>()(lisp_val);
  }
};

// pointers conversion
template <typename CppT>
struct ConvertToCpp<CppT, typename std::enable_if_t<std::is_pointer_v<CppT>>> {
  using LispT = typename static_type_mapping<CppT>::type;
  CppT operator()(LispT lisp_val) const { return unbox<CppT, LispT>(lisp_val); }
};

// complex numbers types
template <typename CppT>
struct ConvertToCpp<CppT, typename std::enable_if_t<is_complex_v<CppT>>> {
  using LispT = typename static_type_mapping<CppT>::type;
  CppT operator()(LispT lisp_val) const { return unbox<CppT, LispT>(lisp_val); }
};

// strings
template <typename CppT>
struct ConvertToCpp<CppT, typename std::enable_if_t<is_std_string_v<CppT>>> {
  using LispT = typename static_type_mapping<CppT>::type;
  std::string operator()(const char *str) const {
    static_assert(std::is_same_v<LispT, const char *>, "type mismatch");
    return std::string(ConvertToCpp<const char *>()(str));
  }
};

// class
template <typename CppT>
struct ConvertToCpp<CppT, typename std::enable_if_t<is_general_class_v<CppT>>> {
  using LispT = typename static_type_mapping<CppT>::type;
  // return reference to avoid extra copy
  CppT &operator()(LispT class_ptr) const {
    static_assert(std::is_same_v<std::remove_const_t<LispT>, void *>,
                  "type mismatch");
    auto cpp_class_ptr = static_cast<CppT *>(class_ptr);
    return *cpp_class_ptr;
  }
};

// Base template for converting To lisp
template <typename CppT, typename Enable = void>
struct ConvertToLisp {
  using type = typename static_type_mapping<CppT>::type;
  template <typename LispT>
  LispT *operator()(CppT &&) {
    static_assert(sizeof(CppT) == 0,
                  "No appropriate specialization for ConvertToLisp");
    return nullptr;
  }
};

template <typename CppT>
struct ConvertToLisp<CppT, typename std::enable_if_t<std::is_void_v<CppT>>> {
  using type = typename static_type_mapping<CppT>::type;
  template <typename LispT>
  void operator()() {
    static_assert(sizeof(CppT) == 0,
                  "No appropriate specialization for ConvertToLisp");
  }
};

// Fundamental type conversion
template <typename CppT>
struct ConvertToLisp<CppT,
                     typename std::enable_if_t<!std::is_void_v<CppT> &&
                                               (std::is_fundamental_v<CppT> ||
                                                std::is_array_v<CppT> ||
                                                is_pod_struct_v<CppT>)>> {
  using type = typename static_type_mapping<CppT>::type;
  CppT operator()(CppT cpp_val) const {
    static_assert(std::is_same_v<type, CppT>, "type mismatch");
    return cpp_val;
  }
};

namespace detail {
template <typename CppT, typename LispT>
struct RefToLisp {
  LispT operator()(CppT cpp_val) const {
    return static_cast<LispT>(std::addressof(cpp_val));
  }
};
}  // namespace detail

// Reference conversion
template <typename CppT>
struct ConvertToLisp<CppT,
                     typename std::enable_if_t<std::is_reference_v<CppT>>> {
  // reference to fundamental type
  using type = typename static_type_mapping<CppT>::type;
  using LispT = typename static_type_mapping<CppT>::type;
  LispT operator()(CppT cpp_val) const {
    static_assert(
        std::is_same_v<type, void *> || std::is_same_v<type, const void *>,
        "type mismatch");
    return detail::RefToLisp<CppT, LispT>()(cpp_val);
  }
};

// pointers conversion
template <typename CppT>
struct ConvertToLisp<CppT, typename std::enable_if_t<std::is_pointer_v<CppT>>> {
  using type = typename static_type_mapping<CppT>::type;
  using LispT = typename static_type_mapping<CppT>::type;
  LispT operator()(CppT cpp_val) const {
    static_assert(std::is_pointer_v<LispT>, "type mismatch");
    return box<CppT, LispT>(cpp_val);
  }
};

// complex numbers types
template <typename CppT>
struct ConvertToLisp<CppT, typename std::enable_if_t<is_complex_v<CppT>>> {
  using type = typename static_type_mapping<CppT>::type;
  using LispT = typename static_type_mapping<CppT>::type;
  LispT operator()(CppT cpp_val) const {
    static_assert(std::is_same_v<LispT, LispComplex>, "type mismatch");
    return box<CppT, LispT>(cpp_val);
  }
};

// Srings
template <typename CppT>
struct ConvertToLisp<CppT, typename std::enable_if_t<is_std_string_v<CppT>>> {
  using type = typename static_type_mapping<CppT>::type;
  const char *operator()(const std::string &str) const {
    static_assert(std::is_same_v<type, const char *>, "type mismatch");
    return ConvertToLisp<const char *>()(str.c_str());
  }
};

// class exclude std::string
template <typename CppT>
struct ConvertToLisp<CppT,
                     typename std::enable_if_t<is_general_class_v<CppT>>> {
  using type = typename static_type_mapping<CppT>::type;
  using LispT = typename static_type_mapping<CppT>::type;
  LispT operator()(CppT cpp_class) const {
    static_assert(std::is_same_v<std::remove_const_t<LispT>, void *>,
                  "type mismatch");

    auto obj_ptr = static_cast<CppT *>(
        MemPool().allocate(sizeof(CppT), std::alignment_of_v<CppT>));
    ::new (obj_ptr) CppT(std::move(cpp_class));
    return static_cast<LispT>(obj_ptr);
  }
};

template <typename T, typename Enable = void>
struct CppTypeAdapter {
  using type = T;
};
template <typename T>
struct CppTypeAdapter<
    T, typename std::enable_if_t<internal::is_general_class_v<T>>> {
  using type = T &;
};

}  // namespace internal

template <typename T>
inline std::string LispType() {
  return internal::static_type_mapping<T>::lisp_type();
}

/////
template <typename T>
using ToLisp_t = typename internal::ConvertToLisp<T>::type;

template <typename T>
using ToCpp_t = typename internal::CppTypeAdapter<T>::type;

/// Conversion to lisp
template <typename CppT>
inline ToLisp_t<CppT> ToLisp(CppT cpp_val) {
  return internal::ConvertToLisp<CppT>()(std::forward<CppT>(cpp_val));
}

/// Conversion to C++
template <typename CppT, typename LispT>
inline ToCpp_t<CppT> ToCpp(LispT &&lisp_val) {
  return internal::ConvertToCpp<CppT>()(std::forward<LispT>(lisp_val));
}

}  // namespace clcxx
