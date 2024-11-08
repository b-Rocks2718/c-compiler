import sys
import os

compiler_flags = [
    "-tokens",
    "-ast",
    "-semantics",
    "-tac",
    "-asm"
]

assembler_flags = [
    "-emu"
]

other_flags = [
    "-out",
    "-s",
    "-bin",
    "-run"
]

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: bcc <file.c> <flags>")
        sys.exit(1)

    args = sys.argv[2:]

    cfile = sys.argv[1]
    name = cfile.split('.')[0]
    asmfile = name + ".s"
    outfile = name + ".out"
    binfile = name + ".bin"

    compiler_args = []
    assembler_args = []
    for arg in args:
        # separate flags
        if arg in compiler_flags:
            compiler_args.append(arg)
        elif arg in assembler_flags:
            assembler_args.append(arg)
        elif arg in other_flags:
            pass
        else:
            print("Unrecognized flag: " + arg)
            print("Valid flags are:")
            for flag in compiler_flags:
                print(flag)
            for flag in assembler_flags:
                print(flag)
            print("-out")
            print("-s")
            sys.exit(1)

    # run the compiler and assembler, exit if either fails
    if os.system("cabal run exes -- " + cfile + ' ' + ' '.join(compiler_args)):
        sys.exit(1)
    if not os.path.exists(asmfile):
        sys.exit(1)
    if os.system("python3 Assembler.py " + asmfile + 
          ' asm_libraries/arithmetic.s ' + 
          ' '.join(assembler_args)):
        sys.exit(1)
    if not os.path.exists(binfile):
        sys.exit(1)
    
    result = 0

    if "-run" in args:
        # run the emulator
        result = os.system("cargo run -- " + binfile) >> 8
    else:
        # copy files to windows side so they can be run by the simulation
        os.system("cp ~/c-compiler/ROMs/*.bin /mnt/c/Users/brook/risc16")
        os.system("cp " + binfile + " /mnt/c/Users/brook/risc16/test.bin")

    if "-out" not in args:
        # remove .out files
        os.system("rm " + outfile)
        if "-emu" not in args:
            os.system("rm asm_libraries/os.out")
    if "-s" not in args:
        # remove the .s file
        os.system("rm " + asmfile)
    if "-bin" not in args:
        # remove the .bin file
        os.system("rm " + binfile)
    
    sys.exit(result)
