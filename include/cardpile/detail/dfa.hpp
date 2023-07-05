//          Copyright Hal Finkel 2023.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)
// SPDX-License-Identifier: BSL-1.0

#ifndef CARDPILE_DETAIL_DFA_HPP_
#define CARDPILE_DETAIL_DFA_HPP_

#include <cardpile/detail/utility.hpp>

#include <algorithm>
#include <array>
#include <concepts>
#include <iostream>
#include <optional>
#include <type_traits>
#include <vector>
#include <utility>

namespace cardpile {
namespace detail {
struct dfa {
  // next[state][char] is the transition to the next state (should such a
  // transition exist).
  std::vector<std::vector<std::optional<std::size_t>>> next;

  // final_[state] provides the token index for which this state is the
  // accepting state.
  std::vector<std::optional<std::size_t>> final_;

  // The number of states currently in the NFA.
  std::size_t num_states;
  
  constexpr dfa() : num_states(0) { }

  constexpr void grow_num_states_to(std::size_t count) {
    num_states = std::max(num_states, count);

    next.resize(num_states);
    for (auto &v : next)
      v.resize(256);

    final_.resize(num_states);
  }

  // Print in a form readable by the 'dot' program to render the DFA as a
  // graph.
  void print_dot(std::ostream &os) const {
    os << "digraph DFA {\n";
    os << "rankdir = LR\n";

    for (std::size_t i = 0; i < num_states; ++i)
      if (final_[i])
        os << i << " [shape = doublecircle]\n";

    for (std::size_t i = 0; i < num_states; ++i) {
      for (std::size_t c = 0; c < 256; ++c)
        if (next[i][c])
          os << i << " -> " << next[i][c].value() << " [label=\"" << (char) c << "\"]\n";
    }

    os << "}\n";
  }
};

// This is a representation of the DFA which is a structural type so that it
// can be used as a non-type template parameter.
template <typename state_t, std::size_t num_states>
struct fixed_dfa {
  constexpr fixed_dfa(const dfa &d) {
    for (std::size_t i = 0; i < num_states; ++i) {
      for (std::size_t c = 0; c < 256; ++c)
        if (d.next[i][c])
          next[i][c] = std::make_pair(true, d.next[i][c].value());

      if (d.final_[i])
        final_[i] = std::make_pair(true, d.final_[i].value());
    }
  }

  // next[state][char] is the transition to the next state (should such a
  // transition exist).
  std::array<std::array<std::pair<bool, state_t>, 256>, num_states> next;

  // final_[state] provides the token index for which this state is the
  // accepting state.
  std::array<std::pair<bool, std::size_t>, num_states> final_;
};
} // namespace detail
} // namespace cardpile

#endif // CARDPILE_DETAIL_DFA_HPP_

