#pragma once

#include <memory_resource>

namespace clcxx {

// allocator
constexpr auto BUF_SIZE = 1 * 1024 * 1024;

class VerboseResource : public std::pmr::memory_resource {
 public:
  explicit VerboseResource(std::pmr::memory_resource *upstream_resource)
      : upstream_resource_(upstream_resource), num_of_bytes_allocated(0) {}

  size_t get_num_of_bytes_allocated() { return num_of_bytes_allocated; }

 private:
  void *do_allocate(size_t bytes, size_t alignment) override {
    num_of_bytes_allocated += bytes;
    return upstream_resource_->allocate(bytes, alignment);
  }

  void do_deallocate(void *p, size_t bytes, size_t alignment) override {
    num_of_bytes_allocated -= bytes;
    upstream_resource_->deallocate(p, bytes, alignment);
  }

  [[nodiscard]] bool do_is_equal(
      const memory_resource &other) const noexcept override {
    return this == &other;
  }

  std::pmr::memory_resource *upstream_resource_;
  size_t num_of_bytes_allocated;
};

[[nodiscard]] VerboseResource &MemPool();

}  // namespace clcxx
