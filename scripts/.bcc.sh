
function bcc {
    # file is the first argument
    file=$1

    # get rid of .c extension
    name=${file%.*}
    
    # remove name so the other flags can be passed to the compiler
    shift 1

    # run the compiler
    cabal run exes -- ${file} $@

    # run the assembler
    python3 Assembler.py ${name}.s asm_libraries/arithmetic.s

    # to use bcc command:
    # put 'source ~/c-compiler/.bcc.sh' in the .bashrc file

    # then 'bcc test.c' should compile test.c
    cptowin
}