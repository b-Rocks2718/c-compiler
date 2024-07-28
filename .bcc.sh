
function bcc {
    # file is the first (and only) argument
    file=$1

    # get rid of .c extension
    name=${file%.*}
    
    # run the compiler
    runhaskell -i/home/b_rocks2718/c-compiler ~/c-compiler/CodeGen.hs ${file}

    # run the assembler
    python3 ~/c-compiler/Assembler.py ${name}.s test_code/arithmetic.s

    # to use bcc command:
    # put 'source ~/c-compiler/.bcc.sh' in the .bashrc file

    # then 'bcc test.c' should compile test.c
}
