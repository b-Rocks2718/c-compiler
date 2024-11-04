# Code Generation

This stage outputs the final assembly code. It does register allocation and formats the list of assembly instructions so that they can be read by my assmbler. 
For now, register allocation just means putting everything on the stack, and always using `r3` and `r4` for binary operations. 
Once I can compile more of C, I will write a register allocator to more efficiently use registers.