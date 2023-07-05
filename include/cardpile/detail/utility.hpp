//          Copyright Hal Finkel 2023.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          https://www.boost.org/LICENSE_1_0.txt)
// SPDX-License-Identifier: BSL-1.0

#ifndef CARDPILE_DETAIL_UTILITY_HPP_
#define CARDPILE_DETAIL_UTILITY_HPP_

#include <algorithm>
#include <concepts>
#include <iostream>
#include <limits>
#include <string_view>
#include <type_traits>
#include <utility>

namespace cardpile {
namespace detail {
// A wrapper for string literals enabling use as a non-type template
// parameter using the technique from:
// https://ctrpeach.io/posts/cpp20-string-literal-template-parameters/
template <std::size_t N>
struct string_literal {
  constexpr string_literal(const char (&str)[N]) {
    std::copy_n(str, N, value);
  }

  constexpr operator std::string_view () const { return value; }

  char value[N];
};

template <std::size_t N>
std::ostream& operator << (std::ostream& os, const string_literal<N> s) {
  os << s.value;
  return os;
}

// Find the index of a type in a pack:
// https://stackoverflow.com/questions/26169198/how-to-get-the-index-of-a-type-in-a-variadic-type-pack
template <typename T, typename... Ts>
struct index_in_pack;

template <typename T, typename... Ts>
struct index_in_pack<T, T, Ts...> :
  std::integral_constant<std::size_t, 0> {};

template <typename T, typename U, typename... Ts>
struct index_in_pack<T, U, Ts...> :
  std::integral_constant<std::size_t, 1 + index_in_pack<T, Ts...>::value> {};

template <typename T, typename... Ts>
constexpr std::size_t index_in_pack_v = index_in_pack<T, Ts...>::value;

// A simple technique from
// https://stackoverflow.com/questions/7038797/automatically-pick-a-variable-type-big-enough-to-hold-a-specified-number
// to pick the smallest kind of unsigned integer to hold the given value.
template <unsigned long int N>
struct smallest_int {
  typedef
    typename std::conditional< N < std::numeric_limits<unsigned char>::max(), unsigned char,
    typename std::conditional< N < std::numeric_limits<unsigned short int>::max(), unsigned short int,
    typename std::conditional< N < std::numeric_limits<unsigned int>::max(), unsigned int,
    unsigned long int>::type>::type>::type
  type;
};

// An implementation of nth_element to get the nth element in a parameter
// pack from
// https://ldionne.com/2015/11/29/efficient-parameter-pack-indexing/
template <std::size_t I, typename T>
struct pack_indexed {
  using type = T;
};

template <typename Is, typename ...Ts>
struct pack_indexer;

template <std::size_t ...Is, typename ...Ts>
struct pack_indexer<std::index_sequence<Is...>, Ts...>
  : pack_indexed<Is, Ts>... { };

template <std::size_t I, typename T>
static pack_indexed<I, T> pack_select(pack_indexed<I, T>);

template <std::size_t I, typename ...Ts>
using nth_pack_element =
  typename decltype(pack_select<I>(
    pack_indexer<std::index_sequence_for<Ts...>, Ts...>{}
  ))::type;
} // namespace detail
} // namespace cardpile

#endif // CARDPILE_DETAIL_UTILITY_HPP_

