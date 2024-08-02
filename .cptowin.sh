# copies files from WSL (where the compiler is) to windows (where Digital is)
function cptowin {
    cp ~/c-compiler/ROMs/*.bin /mnt/c/Users/brook/risc16
    cp ~/c-compiler/test_code/test.bin /mnt/c/Users/brook/risc16/test.bin
}
