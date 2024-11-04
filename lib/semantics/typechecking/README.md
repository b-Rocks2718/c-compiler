# Typechecking

Typechecking is the final pass of the semantic analysis stage. A new AST is generated, this time with information about each expression's type. This stage ensures that types are used correctly. Variables and functions have different types, so something like:
```C
int x = 2;
x(6);
```
will be rejected because `x` is not a function type.

This stage also does implicit casts, allowing code like the following to work:
```C
signed x = 2;
unsigned y = 3;
x + y;
```