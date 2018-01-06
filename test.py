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

    def test_block(self):
        result = run_leema('block')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"z is: 36\n", result['output'])

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

    def test_func_inc(self):
        """Test that we can add a const to a function parameter"""
        result = run_leema('func_inc')
        self.assertEqual(5, result['code'])
        self.assertEqual(b"inc(-4) = -3\n", result['output'])

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

    def test_typevar(self):
        result = run_leema('typevar')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"first: 4, second: b\n", result['output'])

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

    def test_fmatch_list_empty(self):
        result = run_leema('fmatch_list_empty')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"is_empty? false\n", result['output'])

    def test_fmatch_scope_depth(self):
        result = run_leema('fmatch_scope_depth')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"found a! what else is in [#b,#c,] ?\n" +
            b"found b! what else is in [#c,] ?\n" +
            b"found c.\n" +
            b"done\n",
            result['output'])

    def test_failure(self):
        result = run_leema('failure')
        self.assertEqual(249, result['code'])
        self.assertEqual(
            b"Failure: #xis4\n" +
            b"Message: tacos are delicious\n" +
            b"Stack Trace:\n" +
            b"<  main:14\n" +
            b"<  add5:9\n" +
            b"<> add4:3\n" +
            b" > add5:9\n" +
            b" > main:14\n" +
            b" > __init__\n" +
            b"\n",
            result['output'])

    def test_failed_nonlinear(self):
        result = run_leema('failed_nonlinear')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"c failed. no propagate.\n" +
            b"d: str interp: whoa - not linear!\n",
            result['output'])

    def test_destruct(self):
        result = run_leema('destruct')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"date is: destruct::Date(2010,9,8,)\n" +
            b"year: 2010 / month: 9 / day: 8\n",
            result['output'])

    def test_chess960(self):
        result = run_leema('chess960')
        self.assertEqual(0, result['code'])
        output = result['output'].strip().decode("utf-8")
        self.assertEqual(8, len(output))
        self.assertEqual(1, output.count("K"))
        self.assertEqual(1, output.count("Q"))
        self.assertEqual(2, output.count("B"))
        self.assertEqual(2, output.count("N"))
        self.assertEqual(2, output.count("R"))

        pr1 = output.index("R")
        pr2 = output.rindex("R")
        pk = output.index("K")
        self.assertTrue(pr1 < pk)
        self.assertTrue(pk < pr2)

    def test_rgb(self):
        result = run_leema('rgb')
        self.assertEqual(0, result['code'])
        expected = b"color: rgb::Rgb(10,20,30,)\nred: 10\nblue: 30\n"
        self.assertEqual(expected, result['output'])

    def test_read_file(self):
        result = run_leema('read_file')
        self.assertEqual(0, result['code'])
        expected = b"hello leema friend\n\n"
        self.assertEqual(expected, result['output'])


if __name__ == '__main__':
    unittest.main()
