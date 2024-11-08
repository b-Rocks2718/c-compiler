import unittest

import os
import glob

class Test_InvalidTest(unittest.TestCase):
    def test_chapter_1(self):
        self.chapter_n_test(1)

    def test_chapter_2(self):
        self.chapter_n_test(2)

    def test_chapter_3(self):
        self.chapter_n_test(3)

    def test_chapter_4(self):
        self.chapter_n_test(4)

    def test_chapter_5(self):
        self.chapter_n_test(5)

    def test_chapter_6(self):
        self.chapter_n_test(6)

    def test_chapter_7(self):
        self.chapter_n_test(7)

    def test_chapter_8(self):
        self.chapter_n_test(8)

    def test_chapter_9(self):
        self.chapter_n_test(9)
    
    def test_chapter_10(self):
        self.chapter_n_test(10)

    def chapter_n_test(self, n):
        # verify invalid files do not compile
        for directory in glob.glob(f"../writing-a-c-compiler-tests/tests/chapter_{n}/invalid*"):
            for file in glob.glob(directory + "/*.c"):
                self.ensure_fails(file)
            if os.path.exists(directory + "/extra_credit"):
                for file in glob.glob(directory + "/extra_credit/*.c"):
                    self.ensure_fails(file)

        # verify valid files produce the correct result
        for file in glob.glob(f"../writing-a-c-compiler-tests/tests/chapter_{n}/valid/*.c"):
            self.ensure_succeeds(file)
        if os.path.exists(f"../writing-a-c-compiler-tests/tests/chapter_{n}/valid/extra_credit"):
            for file in glob.glob(f"../writing-a-c-compiler-tests/tests/chapter_{n}/valid/extra_credit/*.c"):
                self.ensure_succeeds(file)
    
    def ensure_fails(self, file):
        success = os.system("python3 scripts/bcc.py " + file + " -emu -bin") >> 8
        name = file.split('/')[-2:]
        self.assertEqual(success, 1, "Failed at " + '/'.join(name))

    def ensure_succeeds(self, file):
        success = os.system("python3 scripts/bcc.py " + file + " -emu -bin") >> 8
        name = file.split('/')[-1]
        self.assertEqual(success, 0, "Failed to compile at valid/" + name)
        
        # ensure compilation succeeded
        binfile = file.split('.')[0] + ".bin"
        self.assert_(os.path.exists(binfile))

        # compile with gcc
        os.system("gcc " + file)

        gcc_result = os.system("./a.out") >> 8
        my_result = os.system("cargo run -- " + binfile) >> 8

        self.assertEqual(gcc_result, my_result, "Wrong result at valid/" + name)

        os.system("rm a.out")
        os.system("rm " + binfile)

if __name__ == '__main__':
    unittest.main()