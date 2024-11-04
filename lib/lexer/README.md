# Lexer

The output of the preprocessor is given to the lexer.
The lexer converts a string (a list of characters) into a list of tokens, which makes parsing much simpler.
Keywords, identifiers, numbers, operators, and other symbols all are represented as different variants
of the `Token` sum type.

This is accompished by defining a parser for each token which looks for a specific sequence of characters and consumes them if they match. 
For example, the parser for the `return` keyword called on `"return 7;"` will output the pair `(Ok ReturnTok, "7;")`, but will output `(Err <msg>, "int x = 2;")` when called on `"int x = 2;"` because this string does not begin with a `return` token.
The lexer just combines all the parsers for each token and tries them all until one succeeds, then repeats until the entire text file is converted to tokens.
If all the parsers in the lexer fail, then there's an invalid token and the compiler reports and error.