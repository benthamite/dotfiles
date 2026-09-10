"""Exercise the published launcher without opening a real notes corpus or engine."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

LAUNCHER = Path(__file__).resolve().parents[1] / 'claude/bin/triage-notes'


class LauncherTests(unittest.TestCase):
    def test_missing_configuration_fails_before_engine_or_notes(self):
        with tempfile.TemporaryDirectory() as directory:
            result = subprocess.run([str(LAUNCHER), 'inventory', '/must-not-open'],
                                    env={**os.environ, 'TRIAGE_NOTES_CONFIG': directory + '/missing'},
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 1)
            self.assertIn('No notes were accessed', result.stderr)

    def test_configured_engine_receives_unoverrideable_boundaries(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            engine = root / 'engine.py'
            engine.write_text('import json\ndef main(argv, **kwargs):\n'
                              ' print(json.dumps({"argv": argv, "corpus": kwargs["required_corpus"], '
                              '"excluded": [str(p) for p in kwargs["excluded_roots"]]}))\n return 0\n')
            config = root / 'config.json'
            config.write_text(json.dumps({'schema_version': 1, 'engine': str(engine),
                                          'excluded_roots': [str(root / 'work')]}))
            result = subprocess.run([str(LAUNCHER), '--help'],
                                    env={**os.environ, 'TRIAGE_NOTES_CONFIG': str(config)},
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(json.loads(result.stdout), {'argv': ['--help'], 'corpus': 'personal-notes',
                                                         'excluded': [str((root / 'work').resolve())]})
            self.assertFalse((root / '__pycache__').exists())
            config.write_text(json.dumps({'schema_version': 1, 'engine': str(engine), 'excluded_roots': []}))
            result = subprocess.run([str(LAUNCHER)], env={**os.environ, 'TRIAGE_NOTES_CONFIG': str(config)},
                                    capture_output=True, text=True)
            self.assertEqual(result.returncode, 1)
            self.assertEqual(result.stdout, '')


if __name__ == '__main__':
    unittest.main()
