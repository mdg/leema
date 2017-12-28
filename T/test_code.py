import unittest
import subprocess

def get_leema_code(module_func):
    [mod, func] = module_func.split("::")
    file = "T/"+mod+".lma"
    args = ["target/debug/leema", "--func", func, "code", file]
    print(args)
    proc = subprocess.Popen(args, stdout=subprocess.PIPE)
    result = proc.wait()
    output = proc.stdout.read()
    lines = output.strip().splitlines()
    print(lines)
    return {'code': result, 'ops': output}

class TestCode(unittest.TestCase):

    def test_booland(self):
        code = get_leema_code("identity::foo")
        self.assertEqual(5, ops.code)

