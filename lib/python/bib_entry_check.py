"""Read-only checks of decoded Ebib entries against the shared bibliography policy.

This is not a BibTeX parser or an assessment of bibliographic accuracy.  Required
fields come from the policy document; passing them still requires human or agent
review of work identity, edition, metadata relevance and attachment contents.
"""

from __future__ import annotations

import datetime
import json
import re
from pathlib import Path
from urllib.parse import urlsplit


class CheckError(ValueError):
    """The supplied JSON or policy cannot be checked reliably."""


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise CheckError(f"Duplicate JSON property: {key}")
        result[key] = value
    return result


def read_json(text: str):
    """Read JSON without silently discarding duplicate properties."""
    try:
        return json.loads(text, object_pairs_hook=_unique_object)
    except json.JSONDecodeError as exc:
        raise CheckError(f"Invalid JSON: {exc.msg} at line {exc.lineno}") from exc


def _string_list(value) -> bool:
    return isinstance(value, list) and all(isinstance(item, str) and item for item in value)


def _field_name(value) -> bool:
    return isinstance(value, str) and bool(re.fullmatch(r"[a-z][a-z0-9_-]*", value))


def load_policy(path: Path) -> dict:
    """Load the one fenced JSON block containing ``bib_entry_check``."""
    blocks = re.findall(r"^ {0,3}```json\s*\n(.*?)^ {0,3}```\s*$",
                        path.read_text(encoding="utf-8"), re.MULTILINE | re.DOTALL)
    candidates = []
    for block in blocks:
        if '"bib_entry_check"' in block:
            value = read_json(block)
            if isinstance(value, dict) and "bib_entry_check" in value:
                candidates.append(value["bib_entry_check"])
    if len(candidates) != 1:
        raise CheckError("Policy must contain exactly one bib_entry_check JSON block")
    rules = candidates[0]
    if not isinstance(rules, dict) or type(rules.get("version")) is not int or rules["version"] != 1:
        raise CheckError("Unsupported bibliography policy version")
    if set(rules) != {"version", "types", "field_rules", "crossref_inheritance",
                      "forbidden_fields", "semantic_review_required"}:
        raise CheckError("Policy contains missing or unknown rule sections")
    types = rules.get("types")
    if not isinstance(types, dict) or not types:
        raise CheckError("Policy types must be a nonempty object")
    for name, spec in types.items():
        if (not isinstance(name, str) or not name or name != name.lower()
                or not isinstance(spec, dict)
                or not set(spec) <= {"required", "required_any"}
                or not _string_list(spec.get("required", []))
                or not isinstance(spec.get("required_any", []), list)
                or any(not _string_list(group) or not group
                       for group in spec.get("required_any", []))):
            raise CheckError(f"Invalid required-field policy for {name}")
        fields = spec.get("required", []) + [field for group in spec.get("required_any", []) for field in group]
        if not fields or not all(_field_name(field) for field in fields):
            raise CheckError(f"Policy for {name} needs valid required field names")
    checks = rules.get("field_rules")
    if (not isinstance(checks, dict)
            or not all(_field_name(field) for field in checks)
            or any(not isinstance(name, str) or name not in FIELD_CHECKS for name in checks.values())):
        raise CheckError("Policy contains an unknown field check")
    inheritance = rules.get("crossref_inheritance")
    if (not isinstance(inheritance, dict)
            or not all(_field_name(field) for field in inheritance)
            or any(not _string_list(fields) or not fields or not all(_field_name(field) for field in fields)
                   for fields in inheritance.values())):
        raise CheckError("Policy crossref_inheritance must map fields to nonempty field lists")
    forbidden = rules.get("forbidden_fields")
    if (not isinstance(forbidden, dict)
            or not all(_field_name(field) for field in forbidden)
            or any(not isinstance(reason, str) or not reason for reason in forbidden.values())):
        raise CheckError("Policy forbidden_fields must map fields to reasons")
    if not _string_list(rules.get("semantic_review_required")) or not rules["semantic_review_required"]:
        raise CheckError("Policy must explicitly require semantic review")
    return rules


def _present(value: str) -> bool:
    # Ebib can export empty braced/quoted values; they are not complete metadata.
    return bool(re.sub(r'[{}\s"]', "", value))


def _balanced_braces(value: str) -> bool:
    depth = 0
    escaped = False
    for char in value:
        if escaped:
            escaped = False
        elif char == "\\":
            escaped = True
        elif char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth < 0:
                return False
    return depth == 0


def _date_problem(value: str, *, access=False) -> str | None:
    """Check definite years and ordinary ISO dates without estimating dates."""
    if access:
        if not re.fullmatch(r"\d{4}-\d{2}-\d{2}", value):
            return "Use a full ISO access date (YYYY-MM-DD)"
        try:
            datetime.date.fromisoformat(value)
        except ValueError:
            return "Invalid calendar access date"
        return None
    if not re.search(r"(?<!\d)\d{4}(?!\d)", value):
        return "A supported publication year is missing; do not invent one"
    # Accept date ranges and labelled uncertainty.  More elaborate dates remain
    # an explicit policy limit instead of being interpreted as a plain year.
    for part in value.split("/"):
        if part in ("", ".."):
            continue
        match = re.fullmatch(r"(-?\d{4})(?:-(\d{2})(?:-(\d{2}))?)?[?~%]?", part)
        if not match:
            return "Unsupported date syntax; use ISO year/month/day or a labelled date range"
        year, month, day = match.groups()
        if month and not 1 <= int(month) <= 12:
            return "Invalid calendar month"
        if day:
            calendar_year = int(year)
            leap = calendar_year % 4 == 0 and (calendar_year % 100 != 0 or calendar_year % 400 == 0)
            lengths = [31, 29 if leap else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            if not 1 <= int(day) <= lengths[int(month) - 1]:
                return "Invalid calendar date"
    if value.count("/") > 1:
        return "A date interval has at most two endpoints"
    return None


def _doi_problem(value: str) -> str | None:
    if not re.fullmatch(r"10\.\d{4,9}/\S+", value, re.IGNORECASE):
        return "Use a bare DOI beginning with 10.REGISTRANT/ (not a DOI URL)"
    return None


def _isbn_problem(value: str) -> str | None:
    groups = re.split(r"\s*(?:[,;]|\band\b)\s*", value)
    if len(groups) == 1:
        compact = re.sub(r"[\s-]", "", value)
        if len(compact) not in (10, 13):
            groups = value.split()
    for group in groups:
        isbn = re.sub(r"[\s-]", "", group).upper()
        if re.fullmatch(r"\d{9}[\dX]", isbn):
            valid = sum((10 - index) * (10 if char == "X" else int(char))
                        for index, char in enumerate(isbn)) % 11 == 0
        elif re.fullmatch(r"97[89]\d{10}", isbn):
            valid = sum(int(char) * (1 if index % 2 == 0 else 3)
                        for index, char in enumerate(isbn)) % 10 == 0
        else:
            valid = False
        if not valid:
            return "ISBN format or checksum is invalid; an ISBN must match its specific edition"
    return None


def _url_problem(value: str) -> str | None:
    try:
        parsed = urlsplit(value)
        valid = (parsed.scheme in ("http", "https") and parsed.hostname
                 and not re.search(r"\s", value))
        # Accessing .port also rejects malformed ports without any request.
        parsed.port
    except ValueError:
        valid = False
    return None if valid else "Use an absolute HTTP(S) source URL"


FIELD_CHECKS = {
    "date": _date_problem,
    "access_date": lambda value: _date_problem(value, access=True),
    "doi": _doi_problem,
    "isbn": _isbn_problem,
    "url": _url_problem,
}


def _entries(value, role: str) -> list[dict]:
    values = value if isinstance(value, list) else [value]
    if not values:
        raise CheckError(f"{role} input contains no entries")
    result = []
    for item in values:
        if (not isinstance(item, dict) or not isinstance(item.get("key"), str)
                or not isinstance(item.get("entrytype"), str)
                or not isinstance(item.get("fields"), dict)):
            raise CheckError("Each entry must contain string key/entrytype and an object of fields")
        fields = {}
        for name, field in item["fields"].items():
            if not isinstance(name, str) or not isinstance(field, str):
                raise CheckError(f"Entry {item['key']}: field {name} must be a decoded string")
            name = name.lower()
            if name in fields:
                raise CheckError(f"Entry {item['key']}: duplicate field {name}")
            fields[name] = field.strip()
        result.append({"key": item["key"], "entrytype": item["entrytype"].lower().removeprefix("@"),
                       "fields": fields, "role": role})
    return result


def _crossref_problem(entry: dict, index: dict) -> str | None:
    seen = {entry["key"]}
    current = entry
    while _present(current["fields"].get("crossref", "")):
        key = current["fields"]["crossref"]
        if key in seen:
            return f"Crossref cycle reaches {key}"
        if key not in index:
            return f"Crossref parent {key} was not supplied"
        seen.add(key)
        current = index[key]
    return None


def _effective(entry: dict, field: str, index: dict, inheritance: dict,
               seen=frozenset()) -> tuple[str, dict | None]:
    if entry["key"] in seen:
        return "", None
    value = entry["fields"].get(field, "")
    if _present(value):
        return value, {"key": entry["key"], "field": field}
    parent = index.get(entry["fields"].get("crossref"))
    if parent:
        parent_fields = inheritance.get(field, [])
        # A container's own title outranks its inherited booktitle.  Otherwise
        # a chapter can silently acquire the title of its container's parent.
        for parent_field in parent_fields:
            value = parent["fields"].get(parent_field, "")
            if _present(value):
                return value, {"key": parent["key"], "field": parent_field}
        for parent_field in parent_fields:
            value, origin = _effective(parent, parent_field, index, inheritance,
                                       seen | {entry["key"]})
            if _present(value):
                return value, origin
    return "", None


def _check_files(value: str, roots: list[Path]) -> list[dict]:
    if not _present(value):
        return [{"status": "absent", "reason": "No attachment is recorded"}]
    results = []
    for name in value.split(";"):
        name = name.strip()
        result = {"path": name, "status": "invalid"}
        try:
            if not name:
                raise CheckError("Empty path in the file field")
            path = Path(name).expanduser()
            if not path.is_absolute():
                if not roots:
                    raise CheckError("Relative attachment requires an explicit --file-root")
                candidates = set()
                for root in roots:
                    resolved_root = root.expanduser().resolve()
                    candidate = (resolved_root / path).resolve()
                    if not candidate.is_relative_to(resolved_root):
                        raise CheckError("Relative attachment escapes its explicit file root")
                    if candidate.exists():
                        candidates.add(candidate)
                if len(candidates) != 1:
                    raise CheckError("Relative attachment is missing or matches multiple file roots")
                path = candidates.pop()
            if not path.is_file() or path.stat().st_size == 0:
                raise CheckError("Attachment is missing, empty, or not a regular file")
            if path.suffix.lower() == ".pdf":
                with path.open("rb") as stream:
                    if b"%PDF-" not in stream.read(1024):
                        raise CheckError("Attachment has no PDF header")
            result.update(status="present", resolved_path=str(path), size_bytes=path.stat().st_size)
        except (OSError, ValueError, RuntimeError) as exc:
            result["reason"] = str(exc)
        results.append(result)
    return results


def check_entries(value, rules: dict, *, parents=None, check_files=False,
                  file_roots=()) -> dict:
    """Check supplied entries and parents without modifying them or their files."""
    entries = _entries(value, "entry")
    if parents is not None:
        entries.extend(_entries(parents, "parent"))
    index = {}
    for entry in entries:
        if entry["key"] in index:
            raise CheckError(f"Duplicate supplied entry key: {entry['key']}")
        index[entry["key"]] = entry
    results = []
    for entry in entries:
        fields = entry["fields"]
        result = {"key": entry["key"], "entrytype": entry["entrytype"], "role": entry["role"],
                  "missing_fields": [], "invalid_fields": [], "inherited_fields": {},
                  "file_checks": [], "semantic_review_required": list(rules["semantic_review_required"])}
        invalid = result["invalid_fields"]
        if not entry["key"] or re.search(r"[\s{},\[\];@]", entry["key"]):
            invalid.append({"field": "key", "reason": "Key is empty or contains Org citation delimiters"})
        spec = rules["types"].get(entry["entrytype"])
        if spec is None:
            invalid.append({"field": "entrytype", "reason": "No policy rules for this entry type"})
            spec = {}
        problem = _crossref_problem(entry, index)
        if problem:
            invalid.append({"field": "crossref", "reason": problem})
        elif _present(fields.get("crossref", "")):
            result["semantic_review_required"].append("Verify that the supplied crossref parent is the correct work and edition.")
        required = [[name] for name in spec.get("required", [])] + spec.get("required_any", [])
        for group in required:
            if not any(_present(_effective(entry, field, index, rules["crossref_inheritance"])[0])
                       for field in group):
                result["missing_fields"].append({"any_of": group})
        applicable = set(fields) | set(rules["crossref_inheritance"])
        for field in sorted(applicable):
            value, origin = _effective(entry, field, index, rules["crossref_inheritance"])
            if field in fields and not _field_name(field):
                invalid.append({"field": field, "reason": "Invalid BibLaTeX field name"})
            if field in fields and field in rules["forbidden_fields"]:
                invalid.append({"field": field, "reason": rules["forbidden_fields"][field]})
            if not _present(value):
                continue
            if origin and origin["key"] != entry["key"]:
                result["inherited_fields"][field] = origin
            if not _balanced_braces(value):
                invalid.append({"field": field, "reason": "Unbalanced unescaped braces"})
            if any(ord(char) < 32 and char not in "\n\r\t" for char in value):
                invalid.append({"field": field, "reason": "Unexpected control character"})
            check = rules["field_rules"].get(field)
            if check and (problem := FIELD_CHECKS[check](value)):
                invalid.append({"field": field, "reason": problem})
        if check_files:
            result["file_checks"] = _check_files(fields.get("file", ""), list(file_roots))
            for file_result in result["file_checks"]:
                if file_result["status"] == "invalid":
                    invalid.append({"field": "file", "reason": file_result["reason"], "path": file_result["path"]})
        result["structural_status"] = "fail" if invalid or result["missing_fields"] else "pass"
        results.append(result)
    return {"structural_status": "fail" if any(item["structural_status"] == "fail" for item in results) else "pass",
            "semantic_review_complete": False, "results": results}
