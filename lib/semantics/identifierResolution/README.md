# Identifier Resolution

This stage is responsible for both variable resolution and function resolution. 
For each variable and function used, this stage finds the corresponding declaration, or fails if none exists.
It also ensures that variables do not have duplicate declarations in the same scope, and that global variable are initialized with constant values.
This is accomplished by building a table of all identifiers that have been declared for each scope. 

## Variable Resolution

When this stage sees a new variable declaration, it creates a unique name for it and replaces all uses of the variable with the unique name. 
This is necessary for variable names to be reused in different parts of a program, such as the following:
```C
void test(){
  int x = 2;
}

int main(){
  int x = 7;
  test();
  return x;
}
```
If both uses of `x` accessed the same memory location, we'd be in trouble. The variable resolution pass will rewrite the program like this: 
```C
void test(){
  int x.1 = 2;
}

int main(){
  int x.2 = 7;
  test();
  return x.2;
}
```

Another important part of variable resolution is checking that assignment is only used on `lvalue`s. An `lvalue` is a memory location that can be written to (a variable). This means the assignment `x = 2` is allowed, but `3 = 2` is not, because `3` is not an `lvalue`.

## Function Resolution

Unlike variables, functions are not given unique names. 
This is important so that functions defined in one C file can be called by the same name in other C files. 
If the name of a function is reused and has multiple definitions, the identifier resolution pass fails.