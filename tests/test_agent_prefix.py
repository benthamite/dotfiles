import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "claude" / "bin"))
import agent_prefix  # noqa: E402


@pytest.fixture
def in_forethought(tmp_path, monkeypatch):
    root = tmp_path / "forethought"
    (root / "ph-ps").mkdir(parents=True)
    monkeypatch.setattr(agent_prefix, "FORETHOUGHT", root)
    monkeypatch.chdir(root / "ph-ps")


def test_prefix_added_in_forethought(in_forethought):
    assert agent_prefix.apply("Hola", None, "t") == "[agent] Hola"
    assert agent_prefix.apply("[agent] Hola", None, "t") == "[agent] Hola"


def test_no_prefix_outside_forethought(tmp_path, monkeypatch):
    monkeypatch.setattr(agent_prefix, "FORETHOUGHT", tmp_path / "forethought")
    (tmp_path / "forethought-other").mkdir()
    monkeypatch.chdir(tmp_path / "forethought-other")
    assert agent_prefix.apply("Hola", None, "t") == "Hola"
    assert agent_prefix.apply("[agent] Hola", None, "t") == "Hola"


def test_programmatic_send_goes_unprefixed(in_forethought):
    assert agent_prefix.apply("Hola", None, "t", True) == "Hola"
    assert agent_prefix.apply("[agent] Hola", None, "t", True) == "Hola"


def test_programmatic_and_reviewed_are_alternatives(tmp_path):
    review = tmp_path / "fias.md"
    review.write_text("## Gastón\n\n{>>send<<}\n\n> Hola\n")
    with pytest.raises(SystemExit):
        agent_prefix.apply("Hola", str(review), "t", True)


def test_review_file_text_goes_unprefixed(in_forethought, tmp_path):
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
