//          Copyright Hal Finkel 2023.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)
// SPDX-License-Identifier: BSL-1.0

#ifndef CARDPILE_DETAIL_NFA_HPP_
#define CARDPILE_DETAIL_NFA_HPP_

#include <algorithm>
#include <array>
#include <concepts>
#include <cstdint>
#include <iostream>
#include <optional>
#include <string_view>
#include <type_traits>
#include <vector>
#include <utility>

#include <cardpile/detail/utility.hpp>
#include <cardpile/detail/dfa.hpp>

namespace cardpile {
namespace detail {
// A set of char values (which we're not calling a 'char set' because that term
// has a separate locale-related meaning in this context).
struct set_of_char {
  constexpr bool get(unsigned char x) const {
    return ((ints[x >> 6] >> (x & 0x3F)) & 1) != 0;
  }

  constexpr void set(unsigned char x) {
    ints[x >> 6] |= std::uint64_t(1) << (x & 0x3F);
  }

  constexpr void negate() {
    for (std::uint64_t &i : ints)
      i = ~i;
  }

  std::array<std::uint64_t, 4> ints{};
};

struct nfa {
  // next[state] is a char transition providing a set of chars and a
  // destination state.
  std::vector<std::pair<set_of_char, std::size_t>> next;

  // epsilon[state] is the set of states reachable from state via an
  // epsilon transition.
  std::vector<std::vector<std::size_t>> epsilon;

  // final_[state] provides the token index for which this state is the
  // accepting state.
  std::vector<std::optional<std::size_t>> final_;

  // The number of states currently in the NFA.
  std::size_t num_states;

  constexpr nfa() : num_states(0) {
    // Start with an initial state (everything epsilon-transitions from the
    // initial state).
    grow_num_states_to(1);
  }

  constexpr void insert_regex(std::size_t final_index,
                              std::string_view regex) {
    auto regex_start = regex.begin();
    auto start_end_states = insert_regex(regex_start, regex.end());

    add_epsilon_transition(0, start_end_states.first);
    final_[start_end_states.second] = final_index;
  }

  // A basic subset construction algorithm for converting an NFA to a DFA.
  // The DFA will, in general, not be minimal.
  constexpr void to_dfa(dfa &d) const {
    // This vector of vectors represents a map between DFA states and NFA
    // states. dfa_nfa_states[dfa_state] is a vector of NFA states
    // represented by that DFA state.
    std::vector<std::vector<std::size_t>> dfa_nfa_states;

    // The initial DFA state is the closure of the NFA start state.
    std::vector<size_t> state_closure;
    closure(0, state_closure);
    dfa_nfa_states.push_back(state_closure);
    d.grow_num_states_to(1);

    // The queue of unvisited DFA states. 
    std::vector<std::size_t> q(1, 0);

    while (!q.empty()) {
      std::size_t i = q.back();
      q.pop_back();

      for (std::size_t c = 0; c < 256; ++c) {
        state_closure.clear();

        // For every NFA state in this DFA state, add to its closure the
        // closure of the transitioned-to states for the current character.
        for (std::size_t j : dfa_nfa_states[i])
          if (next[j].first.get(c))
            closure(next[j].second, state_closure);

        // If the closure set is empty, then skip (there is no transition).
        if (state_closure.empty())
          continue;

        auto d_i = std::find(dfa_nfa_states.begin(), dfa_nfa_states.end(),
                             state_closure);
        if (d_i != dfa_nfa_states.end()) {
          // This is one of the existing DFA states, so record the transition.
          std::size_t j = d_i - dfa_nfa_states.begin();
          d.next[i][c] = j;
        } else {
          // This is not a set previously encountered. Make a new DFA state.
          std::size_t j = d.num_states++;

          dfa_nfa_states.push_back(state_closure);
          d.grow_num_states_to(d.num_states);
          d.next[i][c] = j;

          q.push_back(j);
        }
      }

      // If one of the NFA states is a final state, then so is this DFA state.
      for (std::size_t j : dfa_nfa_states[i])
        if (final_[j]) {
          d.final_[i] = final_[j];
          // If there is more than one of these, that's probably user
          // error, but we'll give priority to the earlier tokens.
          break;
        }
    }
  }

  // Print in a form readable by the 'dot' program to render the NFA as a
  // graph.
  void print_dot(std::ostream &os) const {
    os << "digraph NFA {\n";
    os << "rankdir = LR\n";

    for (std::size_t i = 0; i < num_states; ++i)
      if (final_[i])
        os << i << " [shape = doublecircle]\n";

    for (std::size_t i = 0; i < num_states; ++i) {
      for (std::size_t c = 0; c < 256; ++c)
        if (next[i].first.get(c))
          os << i << " -> " << next[i].second << " [label=\"" << (char) c << "\"]\n";

      for (std::size_t j : epsilon[i])
        os << i << " -> " << j << " [style=dotted]\n";
    }

    os << "}\n";
  }

private:
  constexpr void grow_num_states_to(std::size_t count) {
    num_states = std::max(num_states, count);
    next.resize(num_states);
    epsilon.resize(num_states);
    final_.resize(num_states);
  }

  constexpr void add_transition(std::size_t from,
                                unsigned char on, std::size_t to) {
    grow_num_states_to(std::max(from, to) + 1);
    auto &x = next[from];
    x.first.set(on);
    x.second = to;
  }

  constexpr void add_transition(std::size_t from,
                                const set_of_char &on, std::size_t to) {
    grow_num_states_to(std::max(from, to) + 1);
    auto &x = next[from];
    x.first = on;
    x.second = to;
  }

  constexpr void add_epsilon_transition(std::size_t from, std::size_t to) {
    grow_num_states_to(std::max(from, to) + 1);
    epsilon[from].push_back(to);
  }

  // Compute the epsilon closure of a given set of start states.
  constexpr void closure(std::vector<std::size_t> start_states,
                         std::vector<std::size_t> &closure_states) const {
#if defined(__clang__) && defined(_LIBCPP_VERSION)
    // To work around LLVM bug 63761 we cannot use vector::insert to keep a
    // sorted container.
    std::vector<std::size_t> q(start_states);
    while (!q.empty()) {
      std::size_t i = q.back();
      q.pop_back();

      closure_states.push_back(i);

      // Enqueue all states to which there is an epsilon transition (except
      // those already in the closure).
      for (std::size_t j : epsilon[i])
        if (std::find(closure_states.begin(),
            closure_states.end(), j) == closure_states.end())
          q.push_back(j);
    }

    // To ensure that the closure can be identified uniquely, sort before
    // returning (this is needed because of the work around).
    std::sort(closure_states.begin(), closure_states.end());
#else
    std::vector<std::size_t> q(start_states);
    while (!q.empty()) {
      std::size_t i = q.back();
      q.pop_back();

      // Insert the state into the set of states in the closure, keeping the
      // vector sorted for efficient searching.
      closure_states.insert(std::upper_bound(closure_states.begin(),
                                             closure_states.end(), i), i);

      // Enqueue all states to which there is an epsilon transition (except
      // those already in the closure).
      for (std::size_t j : epsilon[i])
        if (!std::binary_search(closure_states.begin(), closure_states.end(), j))
          q.push_back(j);
    }
#endif
  }

  // Compute the epsilon closure of a given start state.
  constexpr void closure(std::size_t state_start,
                         std::vector<std::size_t> &closure_states) const {
    std::vector<std::size_t> start_states(1, state_start);
    closure(start_states, closure_states);
  }

  // Parse regular expression:
  //   regex =   regex '|' regex_seq
  //           | regex_seq
  // Disjunctions have the lowest precedence.
  constexpr std::pair<std::size_t, std::size_t>
  insert_regex(std::string_view::const_iterator &regex_start,
               std::string_view::const_iterator regex_end) {

    auto start_end_states = insert_regex_seq(regex_start, regex_end);

    std::size_t state_start = num_states,
                state_end   = num_states + 1;
    add_epsilon_transition(state_start, start_end_states.first);
    add_epsilon_transition(start_end_states.second, state_end);

    while (regex_start != regex_end && *regex_start == '|') {
      ++regex_start;

      start_end_states = insert_regex_seq(regex_start, regex_end);
      add_epsilon_transition(state_start, start_end_states.first);
      add_epsilon_transition(start_end_states.second, state_end);
    }

    return std::make_pair(state_start, state_end);
  }

  // Parse a regular expression sequence of factors:
  //   regex_seq =   regex_seq regex_factor
  //               | regex_factor
  constexpr std::pair<std::size_t, std::size_t>
  insert_regex_seq(std::string_view::const_iterator &regex_start,
                   std::string_view::const_iterator regex_end) {
    auto start_end_states = insert_regex_factor(regex_start, regex_end);

    while (regex_start != regex_end && *regex_start != '|' && *regex_start != ')') {
      auto start_end_states2 = insert_regex_factor(regex_start, regex_end);

      add_epsilon_transition(start_end_states.second, start_end_states2.first);
      start_end_states.second = start_end_states2.second;
    }

    return start_end_states;
  }

  // Parse a regular expression factor:
  //   regex_factor =   regex_term '*'
  //                  | regex_term '+'
  //                  | regex_term '?'
  //                  | regex_term
  constexpr std::pair<std::size_t, std::size_t>
  insert_regex_factor(std::string_view::const_iterator &regex_start,
                      std::string_view::const_iterator regex_end) {
    auto start_end_states = insert_regex_term(regex_start, regex_end);

    if (regex_start != regex_end && (*regex_start == '*' ||
        *regex_start == '+' || *regex_start == '?')) {

      if (*regex_start == '?') {
        ++regex_start;
        add_epsilon_transition(start_end_states.first, start_end_states.second);
        return start_end_states;
      }

      std::size_t state_start = num_states,
                  state_end   = num_states + 1;

      add_epsilon_transition(state_start, start_end_states.first);
      add_epsilon_transition(start_end_states.second, state_end);

      if (*regex_start == '*')
        add_epsilon_transition(state_start, state_end);

      add_epsilon_transition(start_end_states.second, start_end_states.first);
      
      ++regex_start;

      return std::make_pair(state_start, state_end);
    }
 
    return start_end_states; 
  }

  // Parse a regular expression term:
  //   regex_term =   '[' ... ']'
  //                | '[' '^' ... ']'
  //                | '.'
  //                | '(' regex ')'
  //                | regex_char
  // Note that unlike, for example, POSIX lex, we DO NOT handle escape
  // codes here. Why? Because that functionality is redundant with what C++
  // already provides for string literals (the regular expressions are
  // provided as string literals). If you need, for example, to match a
  // '\n' or a '\t', just put it into the regular expression directly.
  constexpr std::pair<std::size_t, std::size_t>
  insert_regex_term(std::string_view::const_iterator &regex_start,
                    std::string_view::const_iterator regex_end) {
    if (regex_start == regex_end || *regex_start == '|' || *regex_start == ')' ||
        *regex_start == '*' || *regex_start == '+' || *regex_start == '?') {
      // Add a single epsilon transition because this term is empty.
      std::size_t state_start = num_states,
                  state_end   = num_states + 1;
      add_epsilon_transition(state_start, state_end);
      return std::make_pair(state_start, state_end);
    }

    if (*regex_start == '(') {
      auto start_end_states = insert_regex(++regex_start, regex_end);
      ++regex_start; // Skip the closing ')'.
      return start_end_states;
    }

    if (*regex_start == '.') {
      ++regex_start;

      std::size_t state_start = num_states,
                  state_end   = num_states + 1;
      set_of_char char_class;

      // As in POSIX lex, the '.' operator does not match a new line.
      char_class.set('\n');
      char_class.set('\r');
      char_class.negate();

      add_transition(state_start, char_class, state_end);

      return std::make_pair(state_start, state_end);
    }

    if (*regex_start == '[') {
      ++regex_start;

      bool is_neg = false;
      set_of_char char_class;

      if (*regex_start == '^') {
        ++regex_start;
        is_neg = true;
      }

      unsigned char last_char;
      // A ']' character that is first, or first after the '^', is a
      // literal ']'. This is the convention used by POSIX regular
      // expressions. Same is true for a '-'.
      if (*regex_start == ']' || *regex_start == '-') {
        char_class.set(*regex_start);
        last_char = *regex_start;
        ++regex_start;
      }

      bool after_dash = false;
      for (; *regex_start != ']'; ++regex_start) {
        if (after_dash) {
          if (*regex_start != '-')
            char_class.set(*regex_start);

          unsigned char this_char = *regex_start;
          unsigned char range_start = std::min(last_char, this_char),
                        range_end   = std::max(last_char, this_char);
          std::size_t range_size = range_end - range_start;

          // This loop does not include the end points because they're
          // already marked (or, the case of a '-', will be later).
          for (std::size_t i = 1; i < range_size; ++i)
            char_class.set(range_start + i);

          after_dash = *regex_start == '-';
          continue;
        }

        if (*regex_start == '-') {
          after_dash = true;
          continue;
        }

        char_class.set(*regex_start);
        last_char = *regex_start;
      }

      if (after_dash) {
        // The last character in the range was a '-', so add it to the set
        // as a literal.
        char_class.set('-');
      }

      // Skip the closing ']'.
      ++regex_start;

      // For a negated class, flip all of the bits.
      if (is_neg)
        char_class.negate();

      std::size_t state_start = num_states,
                  state_end   = num_states + 1;
      add_transition(state_start, char_class, state_end);
      return std::make_pair(state_start, state_end);
    }

    // Add a single transition for matching a single character.
    std::size_t state_start = num_states,
                state_end   = num_states + 1;
    add_transition(state_start, *regex_start++, state_end);
    return std::make_pair(state_start, state_end);
  }
};
} // namespace detail
} // namespace cardpile

#endif // CARDPILE_DETAIL_NFA_HPP_

