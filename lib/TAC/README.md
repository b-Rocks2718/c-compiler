# TAC

TAC (Three Address Code) is the intermediate representation used my my compiler.
It is a list of instructions, similar to assembly, but at a higher level and without many of the details of a real assembly program. It gets its name because most instructions are of the form `Op <dst> <src1> <src2>`, using three addresses. Instead of worrying about memory locations and registers, the TAC still uses variables. As an example, the assignment
```C
x = 2 * y + 1;
```
will be converted to something like
```
Mul tmp.0 2 y
Add tmp.1 tmp.0 1
Mov x tmp.1
```
Once all the TAC instructions have been generated, the compiler can work out how much space it needs on the stack, and the next stage will replace all the pseudoregisters with real stack locations.

There are a few advantages to using an intermediate representation like this. 
One is that each stage of the compiler has less work to do, making it easier to write.
Another is that many optimizations will be easier to do on a higher level IR like TAC than on real assembly.
When I write an optimization stage, it will operate on the TAC IR.