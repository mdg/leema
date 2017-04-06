import unittest
import subprocess

def run_leema(f):
    args = ["target/debug/leema", "run", "T/"+f+".lma"]
    print(args)
    proc = subprocess.Popen(args, stdout=subprocess.PIPE)
    result = proc.wait()
    output = proc.stdout.read()
    return {'code': result, 'output': output}

class TestScripts(unittest.TestCase):

    def test_booland(self):
        result = run_leema('booland')
        self.assertEqual(0, result['code'])
        lines = result['output'].split(b"\n")
        self.assertEqual(b"a is true", lines[0])
        self.assertEqual(b"b is false", lines[1])
        self.assertEqual(b"a and b is false", lines[2])

    def test_cout(self):
        result = run_leema('cout_quotes')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"hello \"quotes\"\n", result['output'])

    def test_fact_match(self):
        result = run_leema('fact_match')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"factorial(4) = 24\n", result['output'])

    def test_factorial(self):
        result = run_leema('factorial')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"factorial(4) = 24\n", result['output'])

    def test_fizzbuzz(self):
        result = run_leema('fizzbuzz')
        self.assertEqual(0, result['code'])
        lines = result['output'].strip().splitlines()
        self.assertEqual(b"1", lines[0])
        self.assertEqual(b"2", lines[1])
        self.assertEqual(b"fizz", lines[2])
        self.assertEqual(b"4", lines[3])
        self.assertEqual(b"buzz", lines[4])
        self.assertEqual(b"fizz", lines[5])
        self.assertEqual(b"7", lines[6])

        self.assertEqual(b"14", lines[13])
        self.assertEqual(b"fizzbuzz", lines[14])
        self.assertEqual(b"16", lines[15])

        self.assertEqual(b"buzz", lines[-6])
        self.assertEqual(b"fizz", lines[-5])
        self.assertEqual(b"97", lines[-4])
        self.assertEqual(b"98", lines[-3])
        self.assertEqual(b"fizz", lines[-2])
        self.assertEqual(b"buzz", lines[-1])

    def test_func_3params(self):
        result = run_leema('func_3params')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"sum(3, 8, 2) = 13\n", result['output'])

    def test_footag_match(self):
        result = run_leema('footag_match')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"h is #foo\nmatched #foo\n", result['output'])

    def test_footag_nomatch(self):
        result = run_leema('footag_nomatch')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"h is #foot\nsome other thing? #foot\n",
                result['output'])

    def test_if_else_true(self):
        result = run_leema('if_else_true')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"hello\n", result['output'])

    def test_if_else_false(self):
        result = run_leema('if_else_false')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"burritos\n", result['output'])

    def test_if_nested(self):
        result = run_leema('if_nested')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"x = 1, y = 2\n", result['output'])

    def test_list_cons(self):
        result = run_leema('list_cons')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"l1: [3,2,8,]\n" +
            b"l2: [4,6,3,2,8,]\n",
            result['output'])

    def test_list_match_all(self):
        result = run_leema('list_match_all')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"l: [3,2,8,]\n" +
            b"l is a list with 3 elements [3, 2, 8]\n",
            result['output'])

    def test_list_match_head(self):
        result = run_leema('list_match_head')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"l: [3,2,8,4,]\n" +
            b"l is a list starting with 3, 2 and ending with [8,4,]\n",
            result['output'])

    def test_fmatch_list_tail(self):
        result = run_leema('fmatch_list_tail')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"l: [3,2,8,]\n" +
            b"l is a list with head 3 and tail [2,8,]\n",
            result['output'])

    def test_fmatch_scope_depth(self):
        result = run_leema('fmatch_scope_depth')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"found a! what else is in [#b,#c,] ?\n" +
            b"found b! what else is in [#c,] ?\n" +
            b"found c.\n" +
            b"done\n",
            result['output'])

    def test_chess960(self):
        result = run_leema('chess960')
        self.assertEqual(0, result['code'])
        self.assertRegex(result['output'].strip(), b"^[BNRQK]{8}$")

    def test_rgb(self):
        self.skipTest("structs come back later")
        result = run_leema('rgb')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"color: Rgb(10,20,30,)\nred: 10\n", result['output'])


if __name__ == '__main__':
    unittest.main()
