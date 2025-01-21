# My C compiler

My implementation of of the compiler described in [Writing a C Compiler](https://nostarch.com/writing-c-compiler) by Nora Sandler.

This is a C compiler targeting the cpu I'm building.
Since I'm not done building it, I'm testing the compiler on the simulation I have of the cpu in [Digital](https://github.com/hneemann/Digital). At some point I'll make a repository with the cpu schematics. This repository also has an emulator for the cpu written in Rust.

I wrote the assembler in Python a few years ago. It works, but the code is a mess. 
Soon I'll write a better version in Haskell.

## Usage

Run 
```bash
source scripts/bcc.sh
```
from the `c-compiler` directory to be able to use the following commands. 

Compile with

```bash
bcc source.c -flags
```
Compiler flags: `tokens`, `ast`, `semantics`, `tac`, or `asm`. 
You can specify as many of these flags as you want, and the compiler will print
the output of the corresponding stage. 

Assembler flags: `-emu` if you want to target the emulator. The Digital simulation is targeted by default, and the output is written to the folder with the cpu simulation.

Other flags: 
- `-run` if you want to immediately run the emulator after compiling
- `-s` if you want to keep the generated assembly code (it is removed by default)
- `-out` if you want to keep the generated `.out` file
- `-bin` if you want to keep the generated `.bin` file

You can also run 
```bash
basm source.s
```
to assemble an assembly file. `basm` accepts the `-emu`, `-run`, `-s`, `-out`, and `-bin` flags.

Run the emulator with 
```bash
bemu source.bin
```