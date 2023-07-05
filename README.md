# CardPile

## What is CardPile?

CardPile is a header-only modern C++ library that makes heavy use of constexpr (compile-time) metaprogramming to produce highly-optimized code for lexical analysis and other tasks. CardPile requires C++20.

## Lexical Analysis

CardPile contains a "scanner" (a lexical analyzer) which is based on the traditional compilation of regular expressions to an NFA (Nondeterministic Finite Automaton) to a DFA (Deterministic Finite Automaton). In short, it works similarly to POSIX lex except that the compilation process is entirely contained within the library metaprogram.

**A simple scanner example**
```cpp
// We'll use a simple context object which keeps track of the starting and ending iterators of each token.
using context = cardpile::simple_context<std::string::iterator>;

// Some simple tokens (in this case, we don't care too much about the matched text itself).
using true_   = cardpile::kw<"true">;
using ws      = cardpile::token<"[ \t\r\n]+", "ws">;

// A token with more functionality (in this case, an unsigned integer).
struct integer : public cardpile::token<"[1-9][0-9]*", "integer"> {
  integer(context &ctx) : text(ctx.get_start(), ctx.get_end()) { }
  std::string_view text;

  bool get_value(unsigned long &value) const {
    return std::from_chars(text.data(), text.data() + text.size(),
                           value).ec == std::errc{};
  }
};

// Now we'll define the scanner itself by providing the context type and the list of tokens.
using scanner = cardpile::scanner<context,
                  // Tokens:
                  true_,
                  integer,
                  ws
                >;

int main() {
  scanner lex;

  std::string test_input = ...; // We some text from somewhere...

  bool ret = lex.process([](auto t) {
    if constexpr (std::same_as<decltype(t), integer>) {
      unsigned long value;
      if (!t.get_value(value))
        return false;
      // We can now do something with value if we'd like...
    }

    return true;
  }, test_input);

  ...
}

```

## Maturity and Stability

This library is in a "I just got something working" state. No interface or functional stability should be presumed at this point in time. There are no official releases.

## Support

This library is being developed as a hobby of the author. Bugs should be reported using the repository bug tracker. Contributions are welcome.