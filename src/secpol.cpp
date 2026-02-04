#include "secpol.h"
#include <LLVM/Support/raw_ostream.h>

namespace obf0 {
namespace secpol {
allow_policy::allow_policy() {}
allow_policy::allow_policy(std::vector<std::string_view> allowlist) : m_allowlist(std::move(allowlist)) {}
bool allow_policy::is_applicable(std::string_view symbol_name) const {
    for (auto f : m_allowlist) {
        if (f == symbol_name)
            return true;
    }
    return false;
}

block_policy::block_policy() {}
block_policy::block_policy(std::vector<std::string_view> blocklist) : m_blocklist(std::move(blocklist)) {}
bool block_policy::is_applicable(std::string_view symbol_name) const {
    for (auto f : m_blocklist) {
        if (f == symbol_name)
            return false;
    }
    return true;
}

uniform_policy::uniform_policy(bool policy) : m_policy(policy) {}
bool uniform_policy::is_applicable(std::string_view symbol_name) const {
    llvm::errs() << "uniform_policy\n";
    return true;
}
} // namespace secpol
} // namespace obf0