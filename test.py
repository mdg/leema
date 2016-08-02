import unittest
import subprocess

def run_leema(f):
    args = ["target/debug/leema", "T/"+f+".lma"]
    print(args)
    proc = subprocess.Popen(args, stdout=subprocess.PIPE)
    result = proc.wait()
    output = proc.stdout.read()
    return {'code': result, 'output': output}

class TestScripts(unittest.TestCase):
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

    def test_hashtag(self):
        result = run_leema('hashtag')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"h is #foo\nmatched #foo\n", result['output'])

    def test_rgb(self):
        result = run_leema('rgb')
        self.assertEqual(0, result['code'])
        self.assertEqual(b"color: Rgb(10,20,30,)\n", result['output'])


if __name__ == '__main__':
    unittest.main()
