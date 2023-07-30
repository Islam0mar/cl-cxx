#pragma once

#ifdef _WIN32
#ifdef CLCXX_EXPORTS
#define CLCXX_API __declspec(dllexport)
#else
#define CLCXX_API __declspec(dllimport)
#endif
#define CLCXX_ONLY_EXPORTS __declspec(dllexport)
#else
#define CLCXX_API __attribute__((visibility("default")))
#define CLCXX_ONLY_EXPORTS CLCXX_API
#endif

#define CLCXX_VERSION_MAJOR 1
#define CLCXX_VERSION_MINOR 0
#define CLCXX_VERSION_PATCH 0

// From
// https://stackoverflow.com/questions/5459868/concatenate-int-to-string-using-c-preprocessor
#define __CLCXX_STR_HELPER(x) #x
#define __CLCXX_STR(x) __CLCXX_STR_HELPER(x)
#define CLCXX_VERSION_STRING       \
  __CLCXX_STR(CLCXX_VERSION_MAJOR) \
  "." __CLCXX_STR(CLCXX_VERSION_MINOR) "." __CLCXX_STR(CLCXX_VERSION_PATCH)
