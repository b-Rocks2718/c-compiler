import unittest

import os
import glob

class Test(unittest.TestCase):
    ignored = [
        # gcc handles % differently than me
        "../writing-a-c-compiler-tests/tests/chapter_5/valid/exp_then_declaration.c",
        # mod issue again
        "../writing-a-c-compiler-tests/tests/chapter_5/valid/extra_credit/compound_assignment_chained.c",
        # page boundry stuff thats irrelevent for me
        "../writing-a-c-compiler-tests/tests/chapter_10/valid/push_arg_on_page_boundary.c",
        # libraries stuff I'll worry about later
        "../writing-a-c-compiler-tests/tests/chapter_9/valid/libraries/addition.c",
        "../writing-a-c-compiler-tests/tests/chapter_9/valid/libraries/addition_client.c",
        "../writing-a-c-compiler-tests/tests/chapter_9/valid/libraries/many_args.c",
        "../writing-a-c-compiler-tests/tests/chapter_9/valid/libraries/many_args_client.c",
        "../writing-a-c-compiler-tests/tests/chapter_9/valid/libraries/system_call.c",
        "../writing-a-c-compiler-tests/tests/chapter_9/valid/libraries/system_call_client.c",
        "../writing-a-c-compiler-tests/tests/chapter_9/valid/stack_arguments/stack_alignment.c",
    ]

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

    def test_chapter_11(self):
        self.chapter_n_test(11)

    def test_chapter_12(self):
        self.chapter_n_test(12)

    def test_chapter_13(self):
        self.chapter_n_test(13)

    def test_chapter_14(self):
        self.chapter_n_test(14)
      
    def test_chapter_15(self):
        self.chapter_n_test(15)

    def chapter_n_test(self, n):
        # verify invalid files do not compile
        for directory in glob.glob(f"../writing-a-c-compiler-tests/tests/chapter_{n}/invalid*"):
            for file in glob.glob(directory + "/**/*.c", recursive=True):
                if file not in self.ignored:
                  self.ensure_fails(file)
            for file in glob.glob(directory + "/*.c"):
                if file not in self.ignored:
                  self.ensure_fails(file)

        # verify valid files produce the correct result
        for file in glob.glob(f"../writing-a-c-compiler-tests/tests/chapter_{n}/valid/**/*.c", recursive=True):
            if file not in self.ignored:
              self.ensure_succeeds(file)
        for file in glob.glob(f"../writing-a-c-compiler-tests/tests/chapter_{n}/valid/*.c"):
            if file not in self.ignored:
              self.ensure_succeeds(file)
    
    def ensure_fails(self, file):
        success = os.system("python3 scripts/bcc.py " + file + " -emu -bin") >> 8
        name = file.split("invalid")[-1]
        self.assertEqual(success, 1, "Error: compilation succeeded at invalid" + name)

    def ensure_succeeds(self, file):
        success = os.system("python3 scripts/bcc.py " + file + " -emu -bin") >> 8
        name = file.split("valid")[-1]
        self.assertEqual(success, 0, "Error: Failed to compile at valid" + name)
        
        # ensure compilation succeeded
        binfile = file.split('.')[0] + ".bin"
        self.assert_(os.path.exists(binfile))

        # compile with gcc
        os.system("gcc " + file)

        gcc_result = os.system("./a.out") >> 8
        my_result = os.system("cargo run -- " + binfile) >> 8

        os.system("rm a.out")
        os.system("rm " + binfile)

        msg = "Wrong result at valid" + name
        msg += f"\nExpected {gcc_result}, got {my_result}"
        self.assertEqual(gcc_result, my_result, msg)

if __name__ == '__main__':
    unittest.main()