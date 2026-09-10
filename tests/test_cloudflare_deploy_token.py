"""Exercise the real wrapper with synthetic brokers and an offline HTTP transport."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

WRAPPER = Path(__file__).resolve().parents[1] / 'claude/bin/cloudflare-deploy-token'


class CloudflareConfigTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.config = self.root / 'config.json'
        self.values = dict(default_account_id='example_account',
                           automation_admin_reference='op://ExampleAutomation/Admin/credential',
                           desktop_admin_reference='op://ExampleDesktop/Admin/credential')
        self.write_config(self.values)
        self.log = self.root / 'calls.jsonl'
        broker = '''#!/usr/bin/env python3
import json, os, sys
with open(os.environ['MOCK_LOG'], 'a') as log:
    log.write(json.dumps({'broker': os.path.basename(sys.argv[0]), 'args': sys.argv[1:],
                         'stdin': sys.stdin.read() if any(x in sys.argv for x in ('create', 'edit')) else ''}) + '\\n')
if sys.argv[1:3] == ['item', 'get']:
    print(json.dumps({'category': os.environ.get('MOCK_CATEGORY', 'API_CREDENTIAL'), 'title': 'Example title',
                      'fields': [{'id': 'credential', 'value': 'old-synthetic'},
                                 {'id': 'notesPlain', 'value': 'keep notes'}]}))
if sys.argv[1] == 'read':
    if os.environ.get('FAIL_AUTOMATION') and 'ExampleAutomation' in sys.argv[2]:
        sys.exit(1)
    print('synthetic-administrator')
'''
        for name in ('op-automations', 'op-desktop'):
            path = self.root / name
            path.write_text(broker)
            path.chmod(0o700)
        (self.root / 'sitecustomize.py').write_text('''
import io, json, os, urllib.request

def mock_urlopen(request, timeout=None):
    body = json.loads(request.data) if request.data else None
    with open(os.environ['MOCK_LOG'], 'a') as log:
        log.write(json.dumps({'url': request.full_url, 'body': body}) + '\\n')
    if request.method == 'GET':
        result = [{'name': name, 'id': str(i)} for i, name in enumerate(
            ['Workers Scripts Write', 'Workers KV Storage Write', 'D1 Write'])]
    else:
        result = {'value': 'synthetic-runtime-value-for-offline-test'}
    return io.BytesIO(json.dumps({'success': True, 'result': result}).encode())
urllib.request.urlopen = mock_urlopen
''')
        self.env = dict(os.environ, PATH=str(self.root) + os.pathsep + os.environ['PATH'],
                        PYTHONPATH=str(self.root), PYTHONDONTWRITEBYTECODE='1',
                        MOCK_LOG=str(self.log), CLOUDFLARE_DEPLOY_TOKEN_CONFIG=str(self.config))

    def write_config(self, value):
        self.config.write_text(json.dumps(value))
        self.config.chmod(0o600)

    def run_wrapper(self, *args):
        return subprocess.run(['bash', str(WRAPPER), '--project', 'example-project', *args],
                              env=self.env, capture_output=True, text=True, timeout=5)

    def calls(self):
        return [json.loads(line) for line in self.log.read_text().splitlines()]

    def test_default_create_uses_config_and_scopes_without_echoing_secrets(self):
        result = self.run_wrapper()
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = self.calls()
        self.assertEqual(calls[0]['args'], ['read', self.values['automation_admin_reference']])
        policy = calls[2]['body']['policies'][0]
        self.assertEqual(policy['resources'], {'com.cloudflare.api.account.example_account': '*'})
        self.assertEqual(policy['permission_groups'], [{'id': '0'}])
        stored = json.loads(calls[3]['stdin'])
        self.assertEqual(stored['title'], 'Cloudflare API - example-project')
        self.assertEqual(stored['fields'][0]['value'], 'synthetic-runtime-value-for-offline-test')
        self.assertNotIn('synthetic-', result.stdout + result.stderr)
        self.assertEqual([c['broker'] for c in calls[-2:]], ['op-desktop', 'op-automations'])

    def test_override_update_and_explicit_desktop_fallback(self):
        self.env['FAIL_AUTOMATION'] = '1'
        result = self.run_wrapper('--account-id', 'override', '--kv', '--d1', '--update', '--title', 'Example title')
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = self.calls()
        self.assertEqual(calls[2]['args'], ['read', self.values['desktop_admin_reference']])
        policy = calls[4]['body']['policies'][0]
        self.assertEqual(policy['resources'], {'com.cloudflare.api.account.override': '*'})
        self.assertEqual(len(policy['permission_groups']), 3)
        self.assertEqual(calls[5]['args'][:3], ['item', 'edit', 'Example title'])
        stored = json.loads(calls[5]['stdin'])
        self.assertEqual(stored['fields'][1], {'id': 'notesPlain', 'value': 'keep notes'})
        self.assertEqual(stored['fields'][0]['value'], 'synthetic-runtime-value-for-offline-test')
        for call in calls:
            self.assertNotIn('synthetic-runtime-value-for-offline-test', str(call.get('args', [])))
        self.assertIn('trying configured desktop reference', result.stderr)

    def test_update_rejects_unsupported_item_before_minting(self):
        self.env['MOCK_CATEGORY'] = 'LOGIN'
        result = self.run_wrapper('--update')
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(len(self.calls()), 1)

    def test_fifo_and_symlink_configuration_fail_without_blocking(self):
        self.config.unlink()
        os.mkfifo(self.config, 0o600)
        self.assertNotEqual(self.run_wrapper().returncode, 0)
        self.config.unlink()
        target = self.root / 'target.json'
        target.write_text(json.dumps(self.values))
        target.chmod(0o600)
        self.config.symlink_to(target)
        self.assertNotEqual(self.run_wrapper().returncode, 0)
        self.assertFalse(self.log.exists())

    def test_invalid_configuration_never_accesses_brokers_or_provider(self):
        cases = [None, {}, dict(self.values, extra='unexpected'),
                 dict(self.values, default_account_id='$(touch sentinel)'),
                 dict(self.values, automation_admin_reference='op://Example/Admin/field\nextra'),
                 dict(self.values, desktop_admin_reference=42)]
        for value in cases:
            with self.subTest(value=value):
                self.write_config(value)
                result = self.run_wrapper()
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse(self.log.exists())
        self.config.write_text('{malformed private input')
        result = self.run_wrapper()
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn('private input', result.stderr)
        self.config.write_bytes(b'\xff private input')
        result = self.run_wrapper()
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn('private input', result.stderr)
        self.config.unlink()
        self.assertNotEqual(self.run_wrapper().returncode, 0)
        self.write_config(self.values)
        self.config.chmod(0o644)
        self.assertNotEqual(self.run_wrapper().returncode, 0)
        self.assertFalse(self.log.exists())


if __name__ == '__main__':
    unittest.main()
