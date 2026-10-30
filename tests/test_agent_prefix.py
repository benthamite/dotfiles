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
    review.write_text("## Gastón\n\n{==**Borrador**==}{>>send<<}\n\n> Gastón, ya salió\n> la transferencia.\n")
    assert agent_prefix.apply("Gastón, ya salió la transferencia.", str(review), "t") == \
        "Gastón, ya salió la transferencia."


def test_text_missing_from_review_file_is_refused(tmp_path):
    review = tmp_path / "fias.md"
    review.write_text("> Otra cosa\n")
    with pytest.raises(SystemExit):
        agent_prefix.apply("Mejor el mismo repo", str(review), "t")


def test_review_file_text_without_pablos_comment_is_refused(tmp_path):
    review = tmp_path / "fias.md"
    review.write_text("## Eitan\n\n**Borrador**\n\n> Mejor el mismo repo\n\n## Otro\n\n{>>send<<}\n")
    with pytest.raises(SystemExit):
        agent_prefix.apply("Mejor el mismo repo", str(review), "t")
