
function bcc {
    # file is the first (and only) argument
    file=$1

    # get rid of .c extension
    name=${file%.*}
    
    # run the compiler
    runhaskell ~/c-compiler/CodeGen.hs ${file}

    # run the assembler
    python3 ~/c-compiler/Assembler.py ${name}.s

    # to use bcc command:
    # put 'source ~/c-compiler/.bcc.sh' in the .bashrc file

    # then 'bcc test.c' should compile test.c
    # only works while in the 'c-compiler' directory for now
}
