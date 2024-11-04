# Utils

Various utility functions used by the compiler.

## Parser Type

This file defines the `Parser` type used by the lexing and parsing stages.
This type contains a function, `runParser`, which takes a list of tokens and returns a pair
containing the result of the parser and the remaining tokens not consumed by the parser.
The `Applicative` instance for `Parser` allows you to define a parser which chains multiple parsers together and runs them sequentially. The `Alternative` instance allows you to define a parser that tries many parsers and returns the result for the first one that succeeds. The `Monad` instance allows you to write a parser that calls some other parser and then decides what to do based on the result. Together, these instances allow many simple parsers to be combined into complex ones (parser combinators), and are key to defining a parser to build the AST.