"""Mask sixteen-digit credentials without corrupting complete UUID tokens or decimal fractions.

Only the numeric shape heuristic exempts UUIDs. The surrounding pipeline still
applies contextual credential patterns and known-secret fingerprints.
"""

import re
import sys


PATTERN = re.compile(
    r"(?P<uuid>(?<![\w-])[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-"
    r"[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}(?![\w-]))"
    # A run right after a decimal point is the fraction of a number (replay gain,
    # probabilities), not a card or account number: masking it corrupted
    # agents' numeric output (TangoDB worker audit, 15 Nov 2026).
    r"|(?a:(?<![.\d])\b[0-9]{4}[ -]?[0-9]{4}[ -]?[0-9]{4}[ -]?[0-9]{4}\b)"
)


def mask(text: str) -> str:
    return PATTERN.sub(
        lambda match: match.group(0) if match.group("uuid") else "[16_DIGIT_NUMBER_REDACTED]",
        text,
    )


if __name__ == "__main__":
    for line in sys.stdin:
        sys.stdout.write(mask(line))
        sys.stdout.flush()
