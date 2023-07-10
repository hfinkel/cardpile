//          Copyright Hal Finkel 2023.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)
// SPDX-License-Identifier: BSL-1.0

#ifndef CARDPILE_SCANNER_HPP_
#define CARDPILE_SCANNER_HPP_

#include <cardpile/detail/utility.hpp>
#include <cardpile/detail/dfa.hpp>
#include <cardpile/detail/nfa.hpp>

#include <algorithm>
#include <concepts>
#include <iostream>
#include <ranges>
#include <type_traits>
#include <utility>

namespace cardpile {
template <detail::string_literal R, detail::string_literal N>
struct token {
  static constexpr detail::string_literal regex = R, name = N;
};

// A keyword is a simple token where the regular expression is the name.
template <detail::string_literal name>
struct kw : token<name, name> {};

// Base context-type requirements.
template <typename T>
concept context_base = requires (T obj) {
  // Called when the scanner has finished processing a token and is resetting
  // to a state for processing the next one.
  { obj.reset() } -> std::same_as<bool>;

  // Called when the scanner is done processing input.
  { obj.finish() } -> std::same_as<bool>;
};

// Context type requirements that depend on the input type.
template <typename T, typename I>
concept context_input = std::forward_iterator<I> && requires (T obj, I input) {
  // Called when a new input is processed.
  { obj.next(input) } -> std::same_as<bool>;
};

// Context type, including requirements that depend on the input.
template <typename T, typename I>
concept context = context_base<T> && std::forward_iterator<I> && context_input<T, I>;

// A default (null) context object (which does nothing).
struct default_context {
  template <std::forward_iterator I>
  constexpr bool next(I /* input */) { return true; }
  constexpr bool reset() { return true; }
  constexpr bool finish() { return true; }
};

// A simple context object (which stores iterators only).
template <std::forward_iterator I>
struct simple_context {
  constexpr bool next(I input) {
    if (just_reset) {
      just_reset = false;
      start = input;
    }

    current = input;

    return true;
  }

  constexpr bool reset() {
    just_reset = true;
    return true;
  }

  constexpr bool finish() {
    return true;
  }

  constexpr I begin() const {
    return start;
  }

  constexpr I end() const {
    return std::next(current);
  }

  constexpr bool valid() const {
    return !just_reset;
  }

private:
  bool just_reset = true;
  I start, current;
};

template <context_base Ctx, typename... Ts>
struct scanner {
  static constexpr std::size_t number_of_tokens = sizeof...(Ts);

  // A variable template which returns the index of the provided token type.
  // At runtime, the scanner will return tokens using this index.
  template <typename T>
  static constexpr std::size_t token_index_of =
    detail::index_in_pack_v<T, Ts...>;

private:
  // Create the DFA for all of the tokens by creating an NFA and converting
  // to a DFA.
  static void constexpr create_dfa(detail::dfa &d) {
    detail::nfa n;

    // Add all of the provided token regular expressions to the NFA.
    std::size_t i{0};
    ((n.insert_regex(i++, Ts::regex)), ...);

    n.to_dfa(d);
  }

  static constexpr std::size_t num_dfa_states() {
    detail::dfa d;
    create_dfa(d);
    return d.num_states;
  }

public:
  static constexpr std::size_t number_of_states = num_dfa_states();

  // This is the identity of the current DFA state which is manipulated at runtime.
  using state_t = typename detail::smallest_int<number_of_states>::type;

private:
  using fixed_dfa_t = detail::fixed_dfa<state_t, number_of_states>;

  template <fixed_dfa_t fd>
  struct inner_t {
    template <state_t s, typename F, typename CIt>
    struct spec {
      static inline bool try_accept(F &&f, Ctx &ctx) {
        if constexpr (fd.final_[s].first) {
          using token_t = detail::nth_pack_element<fd.final_[s].second, Ts...>;

          bool ret;
          if constexpr (std::constructible_from<token_t, Ctx &>)
            ret = f(token_t(ctx));
          else
            ret = f(token_t());

          return ctx.reset() && ret;
        }

        return false;
      }

      template <unsigned char c>
      static inline bool constexpr process_one(F &&f, state_t &next_state, Ctx &ctx) {
        if constexpr (!fd.next[s][c].first) { // There is no valid transition.
          if (try_accept(std::forward<F>(f), ctx)) {
            if (fd.next[0][c].first) {
              next_state = fd.next[0][c].second;
              return true;
            }
          }

          next_state = 0; // Reset the state.

          // Ignore the return value because this is an error situation anyway.
          (void) ctx.reset();

          return false;
        }

        next_state = fd.next[s][c].second;
        return true;
      }

      template <std::size_t... cs>
      static inline bool constexpr process_one_dispatch(F &&f, CIt c, state_t &next_state, Ctx &ctx,
                                                 std::integer_sequence<std::size_t, cs...>) {
        bool ret;
        constexpr unsigned char z{0};
        std::initializer_list<unsigned char> ({(static_cast<unsigned char>(*c) == static_cast<unsigned char>(cs) ?
                                          (ret = process_one<static_cast<unsigned char>(cs)>(std::forward<F>(f), next_state, ctx)), z :
                                          z)...});
        return ret;
      }

      static inline bool constexpr process_one(F &&f, CIt c, state_t &next_state, Ctx &ctx) {
        return process_one_dispatch(std::forward<F>(f), c, next_state, ctx,
                 std::make_integer_sequence<std::size_t, 256>{});
      }

      static inline bool constexpr finish(F &&f, Ctx &ctx) {
        // This is an accepting state, so we can generate a final token.
        return try_accept(std::forward<F>(f), ctx);
      }
    };

    // Dispatch to the correct specialization, using a technique from
    // https://www.reddit.com/r/cpp/comments/6vyqra/variadic_switch_case/
    template <typename F, typename CIt, state_t... Is>
    inline bool constexpr process_one_dispatch(F &&f, CIt c, Ctx &ctx, std::integer_sequence<state_t, Is...>) {
      bool ret;
      state_t next_state;

      std::initializer_list<state_t> ({(state == Is ?
                                        (ret = spec<Is, F, CIt>::process_one(std::forward<F>(f), c, next_state, ctx)), state_t(0) :
                                        state_t(0))...});

      state = next_state;
      return ret;
    }

    template <typename F, typename CIt>
    inline bool constexpr process_one(F &&f, CIt c, Ctx &ctx) {
      bool ret = process_one_dispatch(std::forward<F>(f), c, ctx,
                   std::make_integer_sequence<state_t, number_of_states>{});

      return ctx.next(c) && ret;
    }

    template <typename F, state_t... Is>
    inline bool constexpr finish_dispatch(F &&f, Ctx &ctx, std::integer_sequence<state_t, Is...>) {
      bool ret;

      std::initializer_list<state_t> ({(state == Is ?
                                        (ret = spec<Is, F, const char * /* unused */>::finish(std::forward<F>(f), ctx)), state_t(0) :
                                        state_t(0))...});

      state = 0; // The state is reset.
      return ret;
    }

    template <typename F>
    inline bool constexpr finish(F &&f, Ctx &ctx) {
      bool ret = finish_dispatch(std::forward<F>(f), ctx,
               std::make_integer_sequence<state_t, number_of_states>{});
      return ctx.finish() && ret;
    }

    state_t state = 0;
  };

  static constexpr fixed_dfa_t get_fixed_dfa() {
    detail::dfa d;
    create_dfa(d);
    return fixed_dfa_t(d);
  }
  
  // This is the inner state object. This is in a nested class to help the
  // compiler only evaluate the DFA creation once (twice, actually, once to
  // figure out how many states there are, and then again to get the DFA data
  // itself).
  inner_t<get_fixed_dfa()> inner;

  // The scanner context object (could be a reference).
  Ctx ctx;

public:
  // Signal that the input is complete, any final token should be processed,
  // and the state should be reset.
  template <typename F>
  inline bool constexpr finish(F &&f) {
    return inner.finish(std::forward<F>(f), ctx);
  }

  // A process all inputs given the starting and ending iterators.
  template <typename F, std::forward_iterator I, std::sentinel_for<I> S = I>
  requires context_input<Ctx, I>
  inline bool constexpr process(F &&f, const I first, const S last, bool partial = false) {
    for (auto i = first; i != last; ++i)
      if (!inner.process_one(std::forward<F>(f), i, ctx))
        return false;

    if (!partial && !inner.finish(std::forward<F>(f), ctx))
      return false;

    return true;
  }

  // Process all of the characters within a provided range.
  template <typename F, std::ranges::forward_range R>
  inline bool constexpr process(F &&f, R r, bool partial = false) {
    return process(std::forward<F>(f), std::ranges::begin(r), std::ranges::end(r), partial);
  }

  // A constructor which takes a context object by reference to initialize
  // the internal context object.
  scanner(Ctx &ctx) : ctx(ctx) { }

  // The default constructor.
  scanner() { }

  // Return a reference to the current context object.
  Ctx &get_context() const {
    return inner.ctx;
  }

  static void info(std::ostream &os) {
    os << "Number of tokens: " << number_of_tokens << "\n";
    os << "\n";

    os << "Tokens:\n";
    ((os << "\t" << Ts::name << ": " << Ts::regex << "\n"), ...);
    os << "\n";

    detail::nfa n;

    std::size_t i{0};
    ((n.insert_regex(i++, Ts::regex)), ...);

    os << "\n";
    n.print_dot(os);

    detail::dfa d;
    n.to_dfa(d);
    d.print_dot(os);
  }
};
} // namespace cardpile

#endif // CARDPILE_SCANNER_HPP_

