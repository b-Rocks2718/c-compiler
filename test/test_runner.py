import unittest

import os
import glob

class Test_InvalidTest(unittest.TestCase):
    def test_invalid(self):
        for file in glob.glob("test/invalid/*.c"):
            success = os.system("python3 scripts/bcc.py " + file + " -emu -bin") >> 8
            self.assertEqual(success, 1)

    def test_return(self):
        success = os.system("python3 scripts/bcc.py test/valid/return_test.c -emu -bin") >> 8
        self.assertEqual(success, 0)
        result = os.system("cargo run -- test/valid/return_test.bin") >> 8
        self.assertEqual(result, 17)
        os.system("rm test/valid/return_test.bin")

if __name__ == '__main__':
    unittest.main()