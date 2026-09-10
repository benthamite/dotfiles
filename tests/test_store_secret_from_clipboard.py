"""Run the clipboard wrapper with synthetic clipboard and storage processes."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

WRAPPER = Path(__file__).resolve().parents[1] / 'claude/bin/store-secret-from-clipboard'
SECRET = 'synthetic-clipboard-credential-for-tests'


class ClipboardStorageTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.log = self.root / 'calls.jsonl'
        self.item = {'id': 'example-item', 'title': 'Example item', 'category': 'API_CREDENTIAL',
                     'fields': [{'id': 'credential', 'value': 'old-synthetic'},
                                {'id': 'notesPlain', 'value': 'retain notes'}]}
        broker = '''#!/usr/bin/env python3
import json, os, sys
with open(os.environ['MOCK_LOG'], 'a') as log:
    log.write(json.dumps({'name': os.path.basename(sys.argv[0]), 'args': sys.argv[1:],
                         'stdin': sys.stdin.read() if sys.argv[1:2] == ['item'] and sys.argv[2] in ('create', 'edit') else ''}) + '\\n')
if sys.argv[1:3] == ['item', 'get']:
    print(os.environ['MOCK_ITEM'])
'''
        for name in ('op-desktop', 'op-automations'):
            path = self.root / name
            path.write_text(broker)
            path.chmod(0o700)
        path = self.root / 'pbpaste'
        path.write_text('#!/usr/bin/env python3\nimport os\nprint(os.environ["MOCK_SECRET"])\n')
        path.chmod(0o700)
        self.env = dict(os.environ, PATH=str(self.root) + os.pathsep + os.environ['PATH'],
                        MOCK_LOG=str(self.log), MOCK_SECRET=SECRET, PYTHONDONTWRITEBYTECODE='1')

    def run_wrapper(self, *args):
        return subprocess.run(['bash', str(WRAPPER), '--vault', 'Automations',
                               '--title', 'Example item', *args],
                              env=dict(self.env, MOCK_ITEM=json.dumps(self.item)),
                              capture_output=True, text=True, timeout=5)

    def calls(self):
        return [json.loads(line) for line in self.log.read_text().splitlines()]

    def test_update_uses_stdin_and_preserves_other_fields(self):
        result = self.run_wrapper('--update')
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = self.calls()
        edit = calls[1]
        self.assertEqual(edit['args'], ['item', 'edit', 'Example item', '--vault', 'Automations'])
        updated = json.loads(edit['stdin'])
        self.assertEqual(updated['fields'][0]['value'], SECRET)
        self.assertEqual(updated['fields'][0]['type'], 'CONCEALED')
        self.assertEqual(updated['fields'][1], self.item['fields'][1])
        self.assertEqual(updated['id'], self.item['id'])
        self.assertNotIn(SECRET, result.stdout + result.stderr)
        for call in calls:
            self.assertNotIn(SECRET, str(call['args']))
        self.assertEqual([call['name'] for call in calls[-2:]], ['op-desktop', 'op-automations'])

    def test_update_adds_missing_custom_field(self):
        result = self.run_wrapper('--update', '--field', 'api-key')
        self.assertEqual(result.returncode, 0, result.stderr)
        updated = json.loads(self.calls()[1]['stdin'])
        self.assertEqual(updated['fields'][:2], self.item['fields'])
        self.assertEqual(updated['fields'][2]['id'], 'api-key')
        self.assertEqual(updated['fields'][2]['value'], SECRET)

    def test_unsupported_or_ambiguous_item_does_not_write(self):
        original = dict(self.item)
        for change in ({'files': [{}]}, {'passkeys': [{}]}, {'fields': None},
                       {'fields': [{'id': 'credential'}, {'label': 'credential'}]}):
            with self.subTest(change=change):
                self.item = dict(original, **change)
                result = self.run_wrapper('--update')
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn(SECRET, result.stdout + result.stderr)
        self.assertTrue(all(call['args'][:2] == ['item', 'get'] for call in self.calls()))

    def test_create_retains_extra_metadata(self):
        result = self.run_wrapper('--extra', 'app_id=EXAMPLE')
        self.assertEqual(result.returncode, 0, result.stderr)
        call = self.calls()[0]
        self.assertEqual(call['args'], ['item', 'create', '--vault', 'Automations', '-'])
        fields = json.loads(call['stdin'])['fields']
        self.assertEqual(fields[0]['value'], SECRET)
        self.assertEqual(fields[1]['value'], 'EXAMPLE')
        self.assertNotIn(SECRET, result.stdout + result.stderr + str(call['args']))


if __name__ == '__main__':
    unittest.main()
