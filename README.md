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

## CMake Variables

In addition to the standard CMake variable, the following CMake variables can be used to configure the library:

| Variable | Type | Default | Description |
| --- | --- | --- | --- |
| `CARDPILE_USE_LIBCXX` | BOOL | OFF | When ON, use select -stdlib=libc++ when compiling and linking. |
| `CARDPILE_SET_COMPILER_RPATH` | BOOL | OFF | When ON, add to the build-time linking RPATH all non-system-default C++ compiler linking directories. This is useful when using a compiler that is not installed in the default system location (and the RPATH is set at build time so that unit tests can run from the build directory). |

## Compiler and Standard-Library Bugs

The development of this library has resulted in the following compiler and standard-library bug reports:

| Project | Bug | Mitigation |
| --- | --- | --- |
| GCC | [Bug 110542](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110542) | Refactored to avoid bug |
| LLVM | [Bug 63761](https://github.com/llvm/llvm-project/issues/63761) | Workaround in code (conditional compilation) |

## Maturity and Stability

This library is in an "I just got something working" state. No interface or functional stability should be presumed at this point in time. There are no official releases.

## Support

This library is being developed as a hobby of the author. Bugs should be reported using the repository bug tracker. Contributions are welcome.
