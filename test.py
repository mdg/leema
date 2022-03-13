#! /usr/bin/python3
import unittest
import subprocess

def run_leema(f, cli_args=None):
    args = ["target/debug/leema", "T/"+f+".lma"]
    env = {"LEEMA_PATH": "lib"}
    if cli_args is not None:
        args += cli_args
    print(args)
    proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)
    output = proc.stdout.read()
    err = proc.stderr.read()
    result = proc.wait(3)
    output = output + proc.stdout.read()
    err = err + proc.stderr.read()
    proc.stdout.close()
    proc.stderr.close()
    return {'code': result, 'output': output, 'stderr': err}

class TestScripts(unittest.TestCase):

    def test_booland(self):
        result = run_leema('booland')
        self.assertEqual(0, result['code'])
        lines = result['output'].split(b"\n")
        self.assertEqual(b"a is True", lines[0])
        self.assertEqual(b"b is False", lines[1])
        self.assertEqual(b"a and b is False", lines[2])

    def test_block(self):
        result = run_leema('block')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"z is: 36\n", result['output'])

    def test_print(self):
        result = run_leema('print_quotes')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"hello \"quotes\"\n", result['output'])

    def test_pmatch_tuples(self):
        result = run_leema('pmatch_tuples')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"both\nfirst\nsecond\nneither\n", result['output'])

    def test_fact_match(self):
        result = run_leema('fact_match')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"factorial(4) = 24\n", result['output'])

    def test_fact_fmatch(self):
        result = run_leema('fact_fmatch')
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
        self.assertEqual(0, result['code'])
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

    def test_generic_func(self):
        result = run_leema('test_generic')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"x: (#burrito,taco,)\n" \
            + b"y: (True,3,)\n" \
            + b"z: (3,#burrito,)\n"
            , result['output'])

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

    def test_str(self):
        result = run_leema('test_str')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"a: aBcDeFg\n" \
            + b"upper: ABCDEFG\n" \
            + b"lower: abcdefg\n"
            , result['output'])

    def test_typevar(self):
        result = run_leema('typevar')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"first: 4, second: b\n", result['output'])

    def test_let_type(self):
        result = run_leema('let_type')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"5 + 3 = 8\n", result['output'])

    def test_list_cons(self):
        result = run_leema('list_cons')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"l1: [3,2,8,]\n" +
            b"l2: [4,6,3,2,8,]\n",
            result['output'])

    def test_list_map(self):
        result = run_leema('test_list_map')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"output: [6,8,10,]\n", result['output'])

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
        self.assertEqual(b"is_empty? False\n", result['output'])

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
        self.skipTest("not reimplemented yet")
        result = run_leema('failure')
        self.assertEqual(249, result['code'])
        self.assertEqual(
            b"Failure(#xis4 'tacos are delicious')\n",
            result['stderr'])

    def test_failed_handled(self):
        self.skipTest("not reimplemented yet")
        result = run_leema('failed_handled')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"c failed. no propagate.\n" +
            b"e: str interp: whoa - not linear!\n",
            result['output'])

    def test_failed_propagated(self):
        self.skipTest("not reimplemented yet")
        result = run_leema('failed_propagated')
        self.assertEqual(249, result['code'])
        self.assertEqual(
            b"c failed. log and propagate\n",
            result['output'])
        self.assertEqual(
            b"Failure(#xis4 'tacos are delicious')\n",
            result['stderr'])

    def test_anon_func(self):
        result = run_leema('test_anon_func')
        self.assertEqual(0, result['code'])
        exp = b"triple i = [3,6,9,12,]\n"
        self.assertEqual(exp, result['output'])

    def test_closures(self):
        result = run_leema('test_closures')
        self.assertEqual(0, result['code'])
        exp = b"multiplied i = [4,8,12,16,]\n"
        self.assertEqual(exp, result['output'])

    def test_destruct(self):
        result = run_leema('destruct')
        self.assertEqual(0, result['code'])
        self.assertEqual(
            b"date is: /destruct/Date(year:2010,month:9,day:8,)\n" +
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

    def test_map(self):
        result = run_leema('test_map')
        self.assertEqual(0, result['code'])
        expected = b"map contains tacos? true\n" \
            + b"map length is 1\n" \
            + b"map[tacos] = </core/Option T:/core/Int>.Some(4,)\n"
        self.assertEqual(expected, result['output'])

    def test_rgb(self):
        result = run_leema('rgb')
        self.assertEqual(0, result['code'])
        expected = b"color: /rgb/Rgb(red:10,green:20,blue:30,)\n" \
            + b"red: 10\n" \
            + b"blue: 30\n" \
            + b"hex green is: #00ff00\n" \
            + b"reddish is: /rgb/Rgb(red:120,green:20,blue:10,)\n"
        self.assertEqual(expected, result['output'])

    def test_const(self):
        result = run_leema('test_const')
        self.assertEqual(0, result['code'])
        lines = result['output'].splitlines()
        self.assertEqual(b"red is: #ff0000", lines[0])

    def test_empty_struct(self):
        result = run_leema('empty_struct')
        self.assertEqual(0, result['code'])
        expected = b"empty: /empty_struct/Empty\n"
        self.assertEqual(expected, result['output'])

    def test_tuple_named_fields(self):
        result = run_leema('tuple_named_fields')
        self.assertEqual(0, result['code'])
        expected = b"""area is: 12\n"""
        self.assertEqual(expected, result['output'])

    def test_named_tuple(self):
        result = run_leema('named_tuple')
        self.assertEqual(0, result['code'])
        expected = b"""greeting is: "/named_tuple/Greeting(hello,world,)"\n"""
        self.assertEqual(expected, result['output'])

    def test_color_enum(self):
        result = run_leema('color_enum')
        self.assertEqual(0, result['code'])
        exp = b"red: Red\n" \
            + b"blue: Blue\n" \
            + b"yellow: Yellow\n"
        self.assertEqual(exp, result['output'])

    def test_option(self):
        result = run_leema('test_option')
        self.assertEqual(0, result['code'])
        exp = b"option a? None\n" \
            + b"no option\n" \
            + b"option b? </core/Option T:/core/Int>.Some(4,)\n" \
            + b"option is 4\n"
        self.assertEqual(exp, result['output'])

    def test_json(self):
        self.skipTest("not ready for testing yet")
        result = run_leema('test_json')
        self.assertEqual(0, result['code'])
        exp = b'6\nfalse\n"hello"\n"#world"\n' \
            + b'["a","b"]\n' \
            + b'{"x":4}\n' \
            + b'{"id":4,"name":"Javier"}\n' \
            + b'coded  \' 9 or true\n' \
            + b'test_json::User(id:8,name:Gerald,)\n'
        self.assertEqual(exp, result['output'])

    def test_cli(self):
        result = run_leema('hi_to')
        self.assertEqual(0, result['code'])
        exp = b'hi world\n'
        self.assertEqual(exp, result['output'])

        result = run_leema('hi_to', ['you'])
        self.assertEqual(0, result['code'])
        exp = b'hi you\n'
        self.assertEqual(exp, result['output'])

        result = run_leema('hi_to', ['tacos and burritos'])
        self.assertEqual(0, result['code'])
        exp = b'hi tacos and burritos\n'
        self.assertEqual(exp, result['output'])

    def test_read_file(self):
        result = run_leema('read_file')
        self.assertEqual(0, result['code'])
        expected = b"hello leema friend\n\n"
        self.assertEqual(expected, result['output'])


if __name__ == '__main__':
    unittest.main()
