import unittest
import subprocess
import time

def run_clientserver(client, server):
    e = {"LEEMA_PATH": "lib"}
    try:
        # start server
        server_args = ["target/debug/leema", "T/"+server+".lma"]
        print(server_args)
        server_proc = \
            subprocess.Popen(server_args, stdout=subprocess.PIPE, env=e)
    except Exception as sse:
        print("server start error: {0}".format(sse))
        server_proc.kill()
        raise sse

    try:
        # start client after pausing to let the server start
        time.sleep(1)
        client_args = ["target/debug/leema", "T/"+client+".lma"]
        print(client_args)
        client_proc = \
            subprocess.Popen(client_args, stdout=subprocess.PIPE, env=e)
    except Exception as cse:
        print("client start error: {0}".format(cse))
        server_proc.kill()
        client_proc.kill()
        raise cse

    try:
        # finish client
        client_result = client_proc.wait(5)
    except subprocess.TimeoutExpired as cto:
        print("client timeout expired: {0}".format(sto))
        server_proc.kill()
        client_proc.kill()
        raise cto
    client_output = client_proc.stdout.read()

    try:
        # finish server
        server_result = server_proc.wait(5)
        print("server finished")
    except subprocess.TimeoutExpired as sto:
        print("server timeout expired: {0}".format(sto))
        server_proc.kill()
        raise sto
    server_output = server_proc.stdout.read()

    return {
        'client_code': client_result, 'client_output': client_output,
        'server_code': server_result, 'server_output': server_output,
    }

def run_leema(f):
    args = ["target/debug/leema", "T/"+f+".lma"]
    e = {"LEEMA_PATH": "lib"}
    print(args)
    proc = subprocess.Popen(args, stdout=subprocess.PIPE, env=e)
    result = proc.wait()
    output = proc.stdout.read()
    return {'code': result, 'output': output}

class TestScripts(unittest.TestCase):

    def test_udp(self):
        result = self.clientserver_success('ping', 'pingin')
        self.assertEqual(
            b"received ping (hello world\n)\n",
            result['server_output'],
        )

    def test_tcp(self):
        result = self.clientserver_success('pingtcp', 'listentcp')
        self.assertEqual(
            b"received: hello tcp world\n\n",
            result['server_output'],
        )
        self.assertEqual(
            b"sent 16 bytes\n",
            result['client_output'],
        )

    def test_hyper_http(self):
        result = run_leema('test_hyper')
        self.assertEqual(0, result['code'])

        lines = result['output'].strip().splitlines()
        line2parts = lines[1].split(b":")
        client_received = line2parts[0]
        lineup = ''.join(sorted(line2parts[1].decode('utf-8')))

        self.assertEqual(
            b"server_run(3998, test_hyper::web_handler)",
            lines[0])
        self.assertEqual(b"client_received", client_received)
        self.assertEqual("BBKNNQRR", lineup)

    def clientserver_success(self, client, server):
        result = run_clientserver(client, server)
        self.assertEqual(0, result['client_code'])
        self.assertEqual(0, result['server_code'])
        return result

if __name__ == '__main__':
    unittest.main()
