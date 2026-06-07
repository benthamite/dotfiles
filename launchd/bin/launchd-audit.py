#!/usr/bin/env python3
import argparse
import json
import plistlib
import re
import subprocess
from pathlib import Path


FIRST_PARTY_PREFIXES = (
    "ai.epoch.",
    "com.benthamite.",
    "com.stafforini.",
    "com.user.",
    "org.tlon.",
    "com.PM2",
)


def expand_path(value):
    return str(Path(value).expanduser()) if value else None


def read_plist(path):
    path = Path(path)
    with path.open("rb") as handle:
        data = plistlib.load(handle)
    return {
        "path": str(path),
        "label": data.get("Label", path.stem),
        "program_arguments": data.get("ProgramArguments", []),
        "start_calendar_interval": data.get("StartCalendarInterval"),
        "stdout": data.get("StandardOutPath"),
        "stderr": data.get("StandardErrorPath"),
    }


def discover_live_plists(directory):
    directory = Path(directory).expanduser()
    live = {}
    if not directory.exists():
        return live
    for path in sorted(directory.glob("*.plist")):
        try:
            info = read_plist(path)
        except Exception as error:
            info = {"path": str(path), "label": path.stem, "error": str(error)}
        live[info["label"]] = info
    return live


def parse_launchctl_labels(output):
    labels = set(re.findall(r"^\s*label = ([^\s]+)\s*$", output, flags=re.MULTILINE))
    labels.update(re.findall(r"^\s*\d+\s+\d+\s+([^\s{}]+)\s*$", output, flags=re.MULTILINE))
    return labels


def discover_launchctl_labels(domain):
    completed = subprocess.run(
        ["launchctl", "print", domain],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        return set()
    return parse_launchctl_labels(completed.stdout)


def registry_labels(registry):
    labels = set()
    for key in ("managed_jobs", "unmanaged_first_party_jobs", "ignored_jobs"):
        labels.update(item["label"] for item in registry.get(key, []))
    return labels


def is_first_party(label):
    return label.startswith(FIRST_PARTY_PREFIXES)


def audit_registry(registry, live_plists, loaded_labels, repo_root):
    repo_root = Path(repo_root)
    findings = []
    known_labels = registry_labels(registry)

    for job in registry.get("managed_jobs", []):
        label = job["label"]
        live_path = expand_path(job.get("live_plist"))
        if label not in live_plists:
            findings.append(
                {
                    "severity": "ERROR",
                    "label": label,
                    "message": "managed job live plist missing",
                    "path": live_path,
                }
            )
        elif label not in loaded_labels:
            findings.append(
                {
                    "severity": "WARN",
                    "label": label,
                    "message": "managed job is not loaded in launchctl",
                    "path": live_plists[label]["path"],
                }
            )

        canonical = job.get("canonical_plist")
        if canonical:
            canonical_path = repo_root / canonical
            if not canonical_path.exists():
                findings.append(
                    {
                        "severity": "ERROR",
                        "label": label,
                        "message": "managed job canonical plist missing",
                        "path": str(canonical_path),
                    }
                )
            elif label in live_plists:
                live_path = Path(live_plists[label]["path"])
                if not live_path.is_symlink() or live_path.resolve() != canonical_path.resolve():
                    findings.append(
                        {
                            "severity": "WARN",
                            "label": label,
                            "message": "managed job live plist is not symlinked to canonical plist",
                            "path": str(live_path),
                        }
                    )

    for label, info in sorted(live_plists.items()):
        if label in known_labels:
            continue
        if is_first_party(label):
            findings.append(
                {
                    "severity": "WARN",
                    "label": label,
                    "message": "first-party live plist is not listed in registry",
                    "path": info["path"],
                }
            )

    return findings


def load_registry(path):
    with Path(path).open(encoding="utf-8") as handle:
        return json.load(handle)


def format_findings(findings):
    if not findings:
        return "OK launchd audit found no issues"
    return "\n".join(
        f"{item['severity']} {item['label']}: {item['message']} ({item['path']})"
        for item in findings
    )


def main(argv=None):
    parser = argparse.ArgumentParser(description="Read-only audit for dotfiles-managed launchd jobs.")
    parser.add_argument(
        "--registry",
        default="launchd/registry/jobs.json",
        help="Path to the launchd job registry JSON.",
    )
    parser.add_argument(
        "--launchagents",
        default="~/Library/LaunchAgents",
        help="LaunchAgents directory to inspect.",
    )
    parser.add_argument(
        "--launchctl-domain",
        default=None,
        help="launchctl domain to inspect; defaults to gui/<uid>.",
    )
    parser.add_argument(
        "--no-launchctl",
        action="store_true",
        help="Skip launchctl loaded-state checks.",
    )
    args = parser.parse_args(argv)

    registry_path = Path(args.registry)
    registry = load_registry(registry_path)
    live_plists = discover_live_plists(args.launchagents)
    if args.no_launchctl:
        loaded_labels = set(live_plists)
    else:
        domain = args.launchctl_domain or f"gui/{subprocess.check_output(['id', '-u'], text=True).strip()}"
        loaded_labels = discover_launchctl_labels(domain)

    findings = audit_registry(
        registry,
        live_plists=live_plists,
        loaded_labels=loaded_labels,
        repo_root=registry_path.resolve().parents[2],
    )
    print(format_findings(findings))
    return 1 if any(item["severity"] == "ERROR" for item in findings) else 0


if __name__ == "__main__":
    raise SystemExit(main())
