#!/opt/homebrew/bin/python3 -B
"""PreToolUse hook: deny references to the retired secrets CLI that 1Password replaced.

The policy is shared with Codex and lives in lib/python/retired_secrets_cli.py.
Matchers in settings.json: Bash and Edit|Write|NotebookEdit.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "lib/python"))

from retired_secrets_cli import main  # noqa: E402

sys.exit(main())
