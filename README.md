# My C compiler

This is a C compiler targeting the cpu I'm building.
Since I'm not done building it, I'm testing the compiler on the simulation I have of the cpu in Digital. 
At some point I'll make a repository with the cpu schematics.

I wrote the assembler in Python a few years ago. It works, but the code is a mess. 
Soon I'll write a better version in Haskell.

## Usage

```bash
bcc source.c
```
or
```bash
bcc source.c --flags
```
where `flags` can be `tokens`, `ast`, `semantics`, `tac`, or `asm`. 
You can specify as many of these flags as you want, and the compiler will print
the output of the corresponding stage. 