#ifndef OBF0_SRC_SECPOL_H
#define OBF0_SRC_SECPOL_H

#include <string_view>
#include <vector>

namespace obf0::secpol
{
	class policy
	{
	   public:
		virtual bool is_applicable( std::string_view symbol_name ) const = 0;
	};

	class allow_policy : public policy
	{
	   public:
		allow_policy();
		allow_policy( std::vector<std::string_view> allowlist );
		bool is_applicable( std::string_view symbol_name ) const;

	   private:
		std::vector<std::string_view> m_allowlist{};
	};

	class block_policy : public policy
	{
	   private:
		std::vector<std::string_view> m_blocklist;

	   public:
		block_policy();
		block_policy( std::vector<std::string_view> blocklist );
		bool is_applicable( std::string_view symbol_name ) const;
	};

	class uniform_policy : public policy
	{
	   public:
		uniform_policy( bool policy = true );
		bool is_applicable( std::string_view symbol_name ) const override;

	   private:
		bool m_policy;
	};
}  // namespace obf0::secpol

#endif  // OBF0_SRC_SECPOL_H
