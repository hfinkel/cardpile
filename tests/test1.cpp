#include <cardpile.hpp>

#include <array>
#include <charconv>
#include <iostream>
#include <string>
#include <string_view>
#include <type_traits>

using context = cardpile::simple_context<std::string::iterator>;
using true_   = cardpile::kw<"true">;
using ws      = cardpile::token<"[ \t\r\n]+", "ws">;
struct integer : public cardpile::token<"[1-9][0-9]*", "integer"> {
  integer(context &ctx) : text(ctx.get_start(), ctx.get_end()) { }
  std::string_view text;

  bool get_value(unsigned long &value) const {
    return std::from_chars(text.data(), text.data() + text.size(),
                           value).ec == std::errc{};
  }
};

using scanner = cardpile::scanner<context,
                  // Tokens:
                  true_,
                  integer,
                  ws
                >;

int main() {
  scanner lex;

  static_assert(lex.number_of_tokens == 3);

  static_assert(lex.token_index_of<true_> == 0);
  static_assert(lex.token_index_of<integer> == 1);
  static_assert(lex.token_index_of<ws> == 2);

  static_assert(lex.number_of_states == 8);
  static_assert(sizeof(decltype(lex)::state_t) == 1);

  std::size_t int_sum = 0;
  std::array<std::size_t, lex.number_of_tokens> token_counts{};

  std::string test_input("42 true 387 true 9 true true");

  bool ret = lex.process([&token_counts, &int_sum, &lex](auto t) {
    ++token_counts[lex.token_index_of<decltype(t)>];
    if constexpr (std::same_as<decltype(t), integer>) {
      unsigned long value;
      if (!t.get_value(value))
        return false;
      int_sum += value;
    }

    return true;
  }, test_input);

  if (token_counts[0] != 4)
    ret = false;
  if (token_counts[1] != 3)
    ret = false;
  if (token_counts[2] != 6)
    ret = false;

  if (int_sum != 438)
    ret = false;

  std::cout << (ret ? "PASSED" : "FAILED") << "\n";
  return !ret;
}

