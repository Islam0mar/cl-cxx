#pragma once

#include <cstdio>
#include <functional>
#include <map>
#include <memory_resource>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include "type_conversion.hpp"

/// helpper for Import function
#define F_PTR(...)                                                            \
  reinterpret_cast<void (*)()>(clcxx::Import([&]() { return __VA_ARGS__; })), \
      __VA_ARGS__

namespace clcxx {

extern "C" typedef struct {
  char *name;
  char *super_classes;
  char *slot_names;
  char *slot_types;
  void (*constructor)();  // null := pod class
  void (*destructor)();   // null := pod class
} ClassInfo;

extern "C" typedef struct {
  char *name;
  bool method_p;
  char *class_obj;
  void (*func_ptr)();
  char *arg_types;
  char *return_type;
} FunctionInfo;

extern "C" typedef struct {
  char *name;
  char *value;
} ConstantInfo;

extern "C" typedef union {
  FunctionInfo Func;
  ClassInfo Class;
  ConstantInfo Const;
} MetaData;

class CLCXX_API Package;

inline void LispError(const char *error);

namespace detail {
template <typename T>
struct is_functional : public std::false_type {};
template <typename T>
struct is_functional<std::function<T>> : public std::true_type {};

template <typename T>
inline constexpr bool is_functional_v = is_functional<T>::value;

template <auto invocable_pointer, typename R, typename... Args>
ToLisp_t<R> DoApply(ToLisp_t<Args>... args) {
  try {
    if constexpr (std::is_invocable_v<decltype(invocable_pointer),
                                      ToCpp_t<Args>...>) {
      if constexpr (std::is_same_v<ToCpp_t<R>, void>) {
        std::invoke(invocable_pointer, ToCpp<Args>(std::move(args))...);
        return;
      } else {
        return ToLisp<R>(
            std::invoke(invocable_pointer, ToCpp<Args>(std::move(args))...));
      }
    } else {
      if constexpr (std::is_same_v<ToCpp_t<R>, void>) {
        std::invoke(*invocable_pointer, ToCpp<Args>(std::move(args))...);
        return;
      } else {
        return ToLisp<R>(
            std::invoke(*invocable_pointer, ToCpp<Args>(std::move(args))...));
      }
    }
  } catch (const std::exception &err) {
    LispError(err.what());
  }
  return ToLisp_t<R>();
}

template <auto std_func_ptr, typename R, typename... Args>
constexpr auto ResolveInvocable(std::function<R(Args...)> *) {
  return &DoApply<std_func_ptr, std::remove_const_t<R>,
                  std::remove_const_t<Args>...>;
}
template <auto func_ptr, typename R, typename... Args>
constexpr auto ResolveInvocable(R (*)(Args...)) {
  return &DoApply<func_ptr, std::remove_const_t<R>,
                  std::remove_const_t<Args>...>;
}

template <auto mem_func_ptr, typename R, typename CT, typename... Args>
constexpr auto ResolveInvocable(R (CT::*)(Args...)) {
  return &DoApply<mem_func_ptr, std::remove_const_t<R>, std::remove_const_t<CT>,
                  std::remove_const_t<Args>...>;
}
template <auto mem_func_ptr, typename R, typename CT, typename... Args>
constexpr auto ResolveInvocable(R (CT::*)(Args...) const) {
  return &DoApply<mem_func_ptr, std::remove_const_t<R>, std::remove_const_t<CT>,
                  std::remove_const_t<Args>...>;
}
template <typename LambdaT, LambdaT *lambda_ptr, typename R, typename... Args>
constexpr auto ResolveInvocableLambda(R (LambdaT::*)(Args...) const) {
  return &DoApply<lambda_ptr, std::remove_const_t<R>,
                  std::remove_const_t<Args>...>;
}

/// mutable lambda
template <typename LambdaT, LambdaT *lambda_ptr, typename R, typename... Args>
constexpr auto ResolveInvocableLambda(R (LambdaT::*)(Args...)) {
  return &DoApply<lambda_ptr, std::remove_const_t<R>,
                  std::remove_const_t<Args>...>;
}
template <auto lambda_ptr>
constexpr auto ResolveInvocable(
    std::enable_if_t<
        std::is_class_v<std::remove_pointer_t<decltype(lambda_ptr)>> &&
            !detail::is_functional_v<
                std::remove_pointer_t<decltype(lambda_ptr)>>,
        decltype(lambda_ptr)>
        p) {
  return ResolveInvocableLambda<std::remove_pointer_t<decltype(p)>, lambda_ptr>(
      &std::remove_pointer_t<decltype(p)>::operator());
}

template <auto x>
inline constexpr auto DecayThenResolve() {
  return ResolveInvocable<std::forward<std::decay_t<decltype(x)>>(x)>(
      std::forward<std::decay_t<decltype(x)>>(x));
}
}  // namespace detail

/**
 * @brief      import a function, method, lambda, or std_function
 *
 * @details    takes a lambda input such as this:
 *             [&](){return <function/method/lambda/std_function> ;}
 *             and convert it to the address of thunk_function which
 *             takes arguments to cxx function.
 *             when method is imported, you have to pass object as
 *             the first argument.
 *             example 1:
 *             given: int foo(float f){return (int)f;}
 *             import: Import([&]() { return foo; })
 *             that would give the address of the thunk function.
 *             example 2:
 *             given: int foo(float f){return (int)f;}
 *             import: Import([&]() { return foo; })(5.123)
 *             that would execute foo(5.123) and return 5.
 *             could be invoked with Import([&]() { return &foo; })
 *             use cases:
 *             Import([&]() { return &MyClass::foo; })(foo_object, args...)
 *             Import([&]() { return &MyClass::operator(); })(foo_object)
 *             Import([&]() { return []() { return "Hello, World\n"; }; })()
 *
 * @param      lambda class
 *
 * @return     function pointer to the thunk fuction
 */
template <typename T>
inline auto Import(T lambda) {
  if constexpr (std::is_class_v<decltype(lambda())>) {
    static auto w = lambda();
    constexpr auto res = detail::DecayThenResolve<&w>();
    return res;
  } else {
    constexpr auto res = detail::DecayThenResolve<lambda()>();
    return res;
  }
}

namespace detail {

char *str_dup(const char *src);
char *str_append(char *old_str, const char *src);

template <typename T>
void remove_c_strings(T obj);

// Base class to specialize for constructor
template <typename CppT, typename... Args>
CppT *CppConstructor(Args... args) {
  auto obj_ptr = static_cast<CppT *>(
      MemPool().allocate(sizeof(CppT), std::alignment_of_v<CppT>));
  ::new (obj_ptr) CppT(args...);
  return obj_ptr;
}

using FuncPtr = void (*)();
template <typename T, bool Constructor = true, typename... Args>
struct CreateClass {
  inline FuncPtr operator()() {
    return reinterpret_cast<void (*)()>(
        clcxx::Import([&]() { return &CppConstructor<T, Args...>; }));
  }
};

template <typename T, typename... Args>
struct CreateClass<T, false, Args...> {
  inline FuncPtr operator()() { return nullptr; }
};

template <typename T>
void free_obj_ptr(void *ptr) {
  auto obj_ptr = static_cast<T *>(ptr);
  obj_ptr->~T();
  MemPool().deallocate(ptr, sizeof(T), std::alignment_of_v<T>);
}

/// handle POD class
template <typename CppT>
std::string arg_type_pod_fix() {
  using T = std::remove_cv_t<CppT>;
  auto return_type = LispType<CppT>();  // case 1
  if constexpr (internal::is_pod_struct_v<T>) {
    if (::clcxx::pod_class_name<T>() == "") {
      throw std::runtime_error("Pod class " + std::string(TypeName<T>()) +
                               " isn't registered and it is passed by value");
    }
  }
  return return_type;
}

/// Make a string with the types in the variadic template parameter pack
template <typename... Args>
std::string arg_types_string() {
  std::vector<std::string> vec = {arg_type_pod_fix<Args>()...};
  std::string s;
  for (auto arg_types : vec) {
    s.append(arg_types);
    s.append("+");
  }
  return s;
}

/// Make a string with the super classes in the variadic template parameter
/// pack
template <typename... Args>
std::string super_classes_string() {
  std::vector<std::string> vec = {general_class_name<Args>()...};
  std::string s;
  for (auto super_types : vec) {
    s.append(super_types);
    s.append("+");
  }
  return s;
}
}  // namespace detail

/// Registry containing different packages
class CLCXX_API PackageRegistry {
 public:
  /// Create a package and register it
  Package &create_package(std::string lpack);

  auto get_package_iter(std::string pack) const {
    const auto iter = p_packages.find(pack);
    if (iter == p_packages.end()) {
      throw std::runtime_error("Pack with name " + pack +
                               " was not found in registry");
    }
    return iter;
  }

  bool has_package(std::string lpack) const {
    return p_packages.find(lpack) != p_packages.end();
  }

  void remove_package(std::string lpack);

  using Iter = std::map<std::string, std::unique_ptr<Package>>::iterator;
  [[nodiscard]] Iter remove_package(Iter iter);

  bool has_current_package() { return p_current_package != nullptr; }
  Package &current_package();
  void reset_current_package() { p_current_package = nullptr; }

  ~PackageRegistry() {
    for (auto it = begin(p_packages); it != end(p_packages);) {
      it = remove_package(it);
    }
  }

  PackageRegistry()
      : p_error_handler_callback(nullptr),
        p_meta_data_handler_callback(nullptr) {}

  void set_error_handler(void (*callback)(char *)) {
    p_error_handler_callback = callback;
  }

  void handle_error(char *err_msg) { p_error_handler_callback(err_msg); }

  void set_meta_data_handler(void (*callback)(MetaData *, uint8_t)) {
    p_meta_data_handler_callback = callback;
  }

  void send_data(MetaData *M, uint8_t n) { p_meta_data_handler_callback(M, n); }

 private:
  std::map<std::string, std::unique_ptr<Package>> p_packages;
  Package *p_current_package = nullptr;
  void (*p_error_handler_callback)(char *);
  void (*p_meta_data_handler_callback)(MetaData *, uint8_t);
};

CLCXX_API PackageRegistry &registry();

inline void LispError(const char *error) {
  clcxx::registry().handle_error(const_cast<char *>(error));
}

template <typename T>
class ClassWrapper;
template <typename T>
class PodClassWrapper;

/// Store all exposed C++ functions associated with a package
class CLCXX_API Package {
 public:
  explicit Package(std::string cl_pack) : p_cl_pack(cl_pack) {}

  /// Define a new function base
  template <typename T>
  void defun(const std::string &name, void (*func_ptr)(), T &&functor,
             bool is_method = false, const char *class_name = "") {
    defun(name, std::forward<T>(functor), is_method, class_name, func_ptr);
  }

  /// Add a class type
  template <typename T, bool Constructor = true, typename... s_classes>
  ClassWrapper<T> defclass(const std::string &name, s_classes...) {
    static_assert(!internal::is_pod_struct_v<T>,
                  "Use defcstruct for pod class types.");
    auto iter = general_class_name.find(Hash32TypeName<T>());
    if (iter != general_class_name.end()) {
      throw std::runtime_error("Class with name " + name +
                               " was already defined in the package, or maybe "
                               "murmur3 string collision!");
    }
    general_class_name[Hash32TypeName<T>()] = name;

    ClassInfo c_info;
    c_info.constructor = detail::CreateClass<T, Constructor>()();
    c_info.destructor = reinterpret_cast<void (*)()>(detail::free_obj_ptr<T>);
    c_info.slot_types = nullptr;
    c_info.slot_names = nullptr;
    c_info.name = detail::str_dup(name.c_str());
    c_info.super_classes =
        detail::str_dup(detail::super_classes_string<s_classes...>().c_str());
    // Store data
    p_classes_meta_data.push_back(c_info);
    return ClassWrapper<T>(*this);
  }

  template <typename T>
  PodClassWrapper<T> defcstruct(const std::string &name) {
    static_assert(internal::is_pod_struct_v<T>,
                  "defcstruct can be used for pod class types only, you should "
                  "defclass.");
    auto iter = pod_class_name.find(Hash32TypeName<T>());
    if (iter != pod_class_name.end()) {
      throw std::runtime_error("struct with name " + name +
                               " was already defined in the package, or maybe "
                               "murmur3 string collision!" +
                               iter->second);
    }
    pod_class_name[Hash32TypeName<T>()] = name;

    ClassInfo c_info;
    c_info.constructor = nullptr;
    c_info.destructor = nullptr;
    c_info.slot_types = nullptr;
    c_info.slot_names = nullptr;
    c_info.name = detail::str_dup(name.c_str());
    c_info.super_classes = nullptr;
    // Store data
    p_classes_meta_data.push_back(c_info);
    return PodClassWrapper<T>(*this);
  }

  /// Set a global constant value at the package level
  template <typename T>
  void defconstant(const std::string &name, T &&value) {
    ConstantInfo const_info;
    const_info.name = detail::str_dup(name.c_str());
    const_info.value = detail::str_dup(std::to_string(value).c_str());
    p_constants.push_back(const_info);
  }

  std::string name() const { return p_cl_pack; }

  const std::unordered_map<SizeT, std::string> &general_classes() const {
    return general_class_name;
  }

  const std::unordered_map<SizeT, std::string> &pod_classes() const {
    return pod_class_name;
  }

  std::vector<ClassInfo> &classes_meta_data() { return p_classes_meta_data; }
  std::vector<FunctionInfo> &functions_meta_data() {
    return p_functions_meta_data;
  }
  std::vector<ConstantInfo> &constants_meta_data() { return p_constants; }

 private:
  /// Define a new function
  template <typename R, typename... Args>
  void defun(const std::string &name, std::function<R(Args...)>, bool is_method,
             const char *class_name, void (*func_ptr)()) {
    FunctionInfo f_info;
    f_info.name = detail::str_dup(name.c_str());
    f_info.method_p = is_method;
    f_info.class_obj = detail::str_dup(class_name);
    f_info.func_ptr = func_ptr;
    f_info.arg_types =
        detail::str_dup(detail::arg_types_string<Args...>().c_str());
    f_info.return_type = detail::str_dup(detail::arg_type_pod_fix<R>().c_str());
    // store data
    p_functions_meta_data.push_back(f_info);
  }

  /// Define a new function. Overload for pointers
  template <typename R, typename... Args>
  void defun(const std::string &name, R (*f)(Args...), bool is_method,
             const char *class_name, void (*func_ptr)()) {
    defun(name, std::function<R(Args...)>(f), is_method, class_name, func_ptr);
  }

  /// Define a new function. Overload for lambda
  template <typename LambdaT>
  void defun(const std::string &name, LambdaT &&lambda, bool is_method,
             const char *class_name, void (*func_ptr)()  // ,
             // std::enable_if_t<!std::is_member_function_pointer_v<LambdaT>,
             //                  bool> = true
  ) {
    static_assert(!std::is_member_function_pointer_v<LambdaT>,
                  "Use defmethod for member functions");
    add_lambda(name, std::forward<LambdaT>(lambda), &LambdaT::operator(),
               is_method, class_name, func_ptr);
  }

  template <typename R, typename LambdaT, typename... ArgsT>
  void add_lambda(const std::string &name, LambdaT &&lambda,
                  R (LambdaT::*)(ArgsT...) const, bool is_method,
                  const char *class_name, void (*func_ptr)()) {
    defun(name, std::function<R(ArgsT...)>(std::forward<LambdaT>(lambda)),
          is_method, class_name, func_ptr);
  }

  template <typename R, typename LambdaT, typename... ArgsT>
  void add_lambda(const std::string &name, LambdaT &&lambda,
                  R (LambdaT::*)(ArgsT...), bool is_method,
                  const char *class_name, void (*func_ptr)()) {
    defun(name, std::function<R(ArgsT...)>(std::forward<LambdaT>(lambda)),
          is_method, class_name, func_ptr);
  }

  std::string p_cl_pack;
  std::vector<ClassInfo> p_classes_meta_data;
  std::vector<FunctionInfo> p_functions_meta_data;
  std::vector<ConstantInfo> p_constants;
  std::unordered_map<SizeT, std::string> general_class_name;
  std::unordered_map<SizeT, std::string> pod_class_name;
  template <class T>
  friend class PodClassWrapper;
  template <class T>
  friend class ClassWrapper;
};

// Helper class to wrap type methods
template <typename T>
class PodClassWrapper {
 public:
  typedef T type;

  explicit PodClassWrapper(Package &pack) : p_package(pack) {}

  // Add public member >> readwrite
  template <typename CT, typename MemberT>
  PodClassWrapper<T> &member(const std::string &name, MemberT CT::*pm) {
    check_member_and_append_its_slots(name, pm);
    return *this;
  }

  // Access to the module
  Package &package() { return p_package; }

 private:
  template <typename CT, typename MemberT>
  constexpr void check_member_and_append_its_slots(const std::string &name,
                                                   MemberT CT::*) {
    static_assert(std::is_base_of<CT, T>::value,
                  "member() requires a class member (or base class member)");
    auto &curr_class = p_package.p_classes_meta_data.back();

    curr_class.slot_types = detail::str_append(
        curr_class.slot_types, std::string(LispType<MemberT>() + "+").c_str());
    curr_class.slot_names = detail::str_append(curr_class.slot_names,
                                               std::string(name + "+").c_str());
  }

  Package &p_package;
};

template <typename T>
class ClassWrapper {
 public:
  typedef T type;

  explicit ClassWrapper(Package &pack) : p_package(pack) {}

  /// Add a constructor with the given argument types
  template <typename... Args>
  ClassWrapper<T> &constructor(std::string name = "") {
    auto &curr_class = p_package.p_classes_meta_data.back();

    if (name == "")
      // Use name as a flag
      name = std::string("create-" + std::string(curr_class.name) +
                         std::to_string(sizeof...(Args)));

    p_package.defun(name, F_PTR(detail::CppConstructor<T, Args...>), false,
                    curr_class.name);
    return *this;
  }

  /// Define a member function
  template <typename FuncT>
  ClassWrapper<T> &defmethod(const std::string &name, void (*func_ptr)(),
                             FuncT &&functor) {
    defmethod(name, std::forward<FuncT>(functor), func_ptr);
    return *this;
  }

  // Add public member >> readwrite
  template <typename CT, typename MemberT>
  ClassWrapper<T> &member(const std::string &name, MemberT CT::*pm) {
    check_member_and_append_its_slots(name, pm);
    defmethod(std::string(name + ".get"),
              F_PTR([pm](const T &c) -> const MemberT { return c.*pm; }));
    defmethod(std::string(name + ".set"),
              F_PTR([pm](T &c, const MemberT val) { c.*pm = val; }));
    return *this;
  }

  // Access to the module
  Package &package() { return p_package; }

 private:
  template <typename CT, typename MemberT>
  constexpr void check_member_and_append_its_slots(const std::string &name,
                                                   MemberT CT::*) {
    static_assert(std::is_base_of<CT, T>::value,
                  "member() requires a class member (or base class member)");
    auto &curr_class = p_package.p_classes_meta_data.back();

    curr_class.slot_types = detail::str_append(
        curr_class.slot_types, std::string(LispType<MemberT>() + "+").c_str());
    curr_class.slot_names = detail::str_append(curr_class.slot_names,
                                               std::string(name + "+").c_str());
  }

  /// Define a member function
  template <typename R, typename CT, typename... ArgsT>
  void defmethod(const std::string &name, R (CT::*f)(ArgsT...),
                 void (*func_ptr)()) {
    auto curr_class = p_package.p_classes_meta_data.back();
    p_package.defun(
        name, [f](T &obj, ArgsT... args) -> R { return (obj.*f)(args...); },
        true, curr_class.name, func_ptr);
  }

  /// Define a member function, const version
  template <typename R, typename CT, typename... ArgsT>
  void defmethod(const std::string &name, R (CT::*f)(ArgsT...) const,
                 void (*func_ptr)()) {
    auto curr_class = p_package.p_classes_meta_data.back();
    p_package.defun(
        name,
        [f](const T &obj, ArgsT... args) -> R { return (obj.*f)(args...); },
        true, curr_class.name, func_ptr);
  }

  /// Define a "member" function using a lambda
  template <typename LambdaT>
  void defmethod(
      const std::string &name, LambdaT &&lambda, void (*func_ptr)(),
      typename std::enable_if<!std::is_member_function_pointer<LambdaT>::value,
                              bool>::type = true) {
    auto curr_class = p_package.p_classes_meta_data.back();
    p_package.defun(name, std::forward<LambdaT>(lambda), true, curr_class.name,
                    func_ptr);
  }

  Package &p_package;
};

template <typename T>
inline std::string general_class_name() {
  auto classes = registry().current_package().general_classes();
  auto iter = classes.find(Hash32TypeName<T>());
  return iter == classes.end() ? "" : iter->second;
}

template <typename T>
inline std::string pod_class_name() {
  auto classes = registry().current_package().pod_classes();
  auto iter = classes.find(Hash32TypeName<T>());
  return iter == classes.end() ? "" : iter->second;
}

}  // namespace clcxx
extern "C" {
CLCXX_API bool clcxx_init(void (*error_handler)(char *),
                          void (*reg_data_callback)(clcxx::MetaData *,
                                                    uint8_t));
CLCXX_API bool remove_package(const char *pack_name);
CLCXX_API bool register_package(const char *cl_pack,
                                void (*regfunc)(clcxx::Package &));
CLCXX_API size_t used_bytes_size();
CLCXX_API size_t max_stack_bytes_size();
CLCXX_API bool delete_string(char *string);
}

#define CLCXX_PACKAGE extern "C" CLCXX_ONLY_EXPORTS void
