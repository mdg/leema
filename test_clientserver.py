import unittest
import subprocess
import time

def run_clientserver(client, server):
    try:
        # start server
        server_args = ["target/debug/leema", "run", "T/"+server+".lma"]
        print(server_args)
        server_proc = subprocess.Popen(server_args, stdout=subprocess.PIPE)
    except Exception as sse:
        print("server start error: {0}".format(sse))
        server_proc.kill()
        raise sse

    try:
        # start client after pausing to let the server start
        time.sleep(1)
        client_args = ["target/debug/leema", "run", "T/"+client+".lma"]
        print(client_args)
        client_proc = subprocess.Popen(client_args, stdout=subprocess.PIPE)
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

class TestScripts(unittest.TestCase):

    def test_udp(self):
        result = self.clientserver_success('ping', 'pingin')
        self.assertEqual(
            b"received ping (hello world\n)\n",
            result['server_output'],
        )

    def clientserver_success(self, client, server):
        result = run_clientserver(client, server)
        self.assertEqual(0, result['client_code'])
        self.assertEqual(0, result['server_code'])
        return result

if __name__ == '__main__':
    unittest.main()
