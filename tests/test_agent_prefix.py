import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "claude" / "bin"))
import agent_prefix  # noqa: E402


def test_prefix_added_without_review_file():
    assert agent_prefix.apply("Hola", None, "t") == "[agent] Hola"
    assert agent_prefix.apply("[agent] Hola", None, "t") == "[agent] Hola"


def test_review_file_text_goes_unprefixed(tmp_path):
    review = tmp_path / "fias.md"
    review.write_text("**Borrador**\n\n> Gastón, ya salió\n> la transferencia.\n")
    assert agent_prefix.apply("Gastón, ya salió la transferencia.", str(review), "t") == \
        "Gastón, ya salió la transferencia."


def test_text_missing_from_review_file_is_refused(tmp_path):
    review = tmp_path / "fias.md"
    review.write_text("> Otra cosa\n")
    with pytest.raises(SystemExit):
        agent_prefix.apply("Mejor el mismo repo", str(review), "t")
