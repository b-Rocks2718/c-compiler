import sys
import os

assembler_flags = [
    "-emu"
]

other_flags = [
    "-out",
    "-bin",
    "-run"
]

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: basm <file.s> <flags>")
        sys.exit(1)

    args = sys.argv[2:]

    asmfile = sys.argv[1]
    name = asmfile.split('.')[0]
    outfile = name + ".out"
    binfile = name + ".bin"

    compiler_args = []
    assembler_args = []
    for arg in args:
        # separate flags
        if arg in assembler_flags:
            assembler_args.append(arg)
        elif arg in other_flags:
            pass
        else:
            print("Unrecognized flag: " + arg)
            print("Valid flags are:")
            for flag in assembler_flags:
                print(flag)
            print("-out")
            print("-s")
            sys.exit(1)

    # run the assembler, exit if it fails
    if os.system("python3 Assembler.py " + asmfile + 
          ' asm_libraries/arithmetic.s ' + 
          ' '.join(assembler_args)):
        # exit if nonzero error code
        sys.exit(1)
    if not os.path.exists(binfile):
       sys.exit(1)
    
    if "-run" in args:
        # run the emulator
        os.system("cargo run -- " + binfile)
    elif "-emu" not in args:
        # copy files to windows side so they can be run by the simulation
        os.system("cp ~/c-compiler/ROMs/*.bin /mnt/c/Users/'Brooks Bryant'/risc16")
        os.system("cp " + binfile + " /mnt/c/Users/'Brooks Bryant'/risc16/test.bin")

    if "-out" not in args:
        # remove .out files
        os.system("rm " + outfile)
        if "-emu" not in args:
            os.system("rm asm_libraries/os.out")
    if "-bin" not in args:
        # remove the .bin file
        os.system("rm " + binfile)
    
