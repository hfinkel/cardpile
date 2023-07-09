//          Copyright Hal Finkel 2023.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)
// SPDX-License-Identifier: BSL-1.0

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

static bool test_r(scanner &lex) {
  std::size_t int_sum = 0;
  std::array<std::size_t, scanner::number_of_tokens> token_counts{};

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

  return ret;
}

static bool test_i(scanner &lex) {
  std::size_t int_sum = 0;
  std::array<std::size_t, scanner::number_of_tokens> token_counts{};

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
  }, test_input.begin(), test_input.end());

  if (token_counts[0] != 4)
    ret = false;
  if (token_counts[1] != 3)
    ret = false;
  if (token_counts[2] != 6)
    ret = false;

  if (int_sum != 438)
    ret = false;

  return ret;
}

int main() {
  scanner lex;

  static_assert(lex.number_of_tokens == 3);

  static_assert(lex.token_index_of<true_> == 0);
  static_assert(lex.token_index_of<integer> == 1);
  static_assert(lex.token_index_of<ws> == 2);

  static_assert(lex.number_of_states == 8);
  static_assert(sizeof(decltype(lex)::state_t) == 1);

  bool ret = test_i(lex);
  if (ret)
    ret = test_r(lex);

  std::cout << (ret ? "PASSED" : "FAILED") << "\n";
  return !ret;
}

