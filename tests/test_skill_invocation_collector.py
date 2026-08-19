from __future__ import annotations

import importlib.machinery
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stderr
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = (
    ROOT
    / ".codex"
    / "skills"
    / "skill-audit"
    / "scripts"
    / "audit_invocations.py"
)


def load_script():
    loader = importlib.machinery.SourceFileLoader("skill_invocation_collector", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


COLLECTOR = load_script()


def record(timestamp: str, record_type: str, payload: dict) -> dict:
    return {"timestamp": timestamp, "type": record_type, "payload": payload}


def message(
    timestamp: str,
    role: str,
    text: str,
    *,
    turn_id: str | None = None,
) -> dict:
    payload = {
        "type": "message",
        "role": role,
        "content": [{"type": "input_text", "text": text}],
    }
    if turn_id:
        payload["internal_chat_message_metadata_passthrough"] = {"turn_id": turn_id}
    return record(timestamp, "response_item", payload)


def legacy_call(timestamp: str, turn_id: str, call_id: str, command: str) -> dict:
    return record(
        timestamp,
        "response_item",
        {
            "type": "function_call",
            "name": "exec_command",
            "call_id": call_id,
            "arguments": json.dumps({"cmd": command}),
            "internal_chat_message_metadata_passthrough": {"turn_id": turn_id},
        },
    )


def legacy_output(timestamp: str, call_id: str, output: str) -> dict:
    return record(
        timestamp,
        "response_item",
        {
            "type": "function_call_output",
            "call_id": call_id,
            "output": output,
        },
    )


def current_call(timestamp: str, turn_id: str, call_id: str, command: str) -> dict:
    return record(
        timestamp,
        "response_item",
        {
            "type": "custom_tool_call",
            "name": "exec",
            "call_id": call_id,
            "input": (
                "const r = await tools.exec_command({"
                f'"cmd":{json.dumps(command)}'
                "}); text(r.output);"
            ),
            "internal_chat_message_metadata_passthrough": {"turn_id": turn_id},
        },
    )


def current_output(timestamp: str, call_id: str, output: str) -> dict:
    return record(
        timestamp,
        "response_item",
        {
            "type": "custom_tool_call_output",
            "call_id": call_id,
            "output": [{"type": "input_text", "text": output}],
        },
    )


class SkillInvocationCollectorTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        self.cutoff = COLLECTOR.parse_timestamp("2026-08-19T12:00:00Z")

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def write_rollout(self, relative_path: str, records: list[dict]) -> Path:
        path = self.root / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(
            "".join(json.dumps(item, sort_keys=True) + "\n" for item in records),
            encoding="utf-8",
        )
        return path

    def turn_records(
        self,
        *,
        turn_id: str,
        user_text: str,
        assistant_text: str,
        skill: str,
        call_id: str,
    ) -> list[dict]:
        return [
            record(
                "2026-08-19T10:00:00Z",
                "turn_context",
                {"turn_id": turn_id},
            ),
            message("2026-08-19T10:00:01Z", "user", user_text),
            message(
                "2026-08-19T10:00:02Z",
                "assistant",
                assistant_text,
                turn_id=turn_id,
            ),
            legacy_call(
                "2026-08-19T10:00:03Z",
                turn_id,
                call_id,
                f"sed -n '1,240p' /tmp/.codex/skills/{skill}/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T10:00:04Z",
                call_id,
                "Process exited with code 0\nOutput:\n"
                f"---\nname: {skill}\ndescription: fixture\n---\nbody\n",
            ),
        ]

    def test_confirms_legacy_explicit_invocation_and_deduplicates_turn(self) -> None:
        records = self.turn_records(
            turn_id="turn-explicit",
            user_text="Please run /diagnose on this incident.",
            assistant_text="I will use the `diagnose` skill to inspect the incident.",
            skill="diagnose",
            call_id="call-1",
        )
        records.extend(
            [
                legacy_call(
                    "2026-08-19T10:00:05Z",
                    "turn-explicit",
                    "call-2",
                    "cat /tmp/.codex/skills/diagnose/SKILL.md",
                ),
                legacy_output(
                    "2026-08-19T10:00:06Z",
                    "call-2",
                    "---\nname: diagnose\ndescription: fixture\n---\nbody\n",
                ),
            ]
        )
        path = self.write_rollout("active/session.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual(1, report["summary"]["confirmed_invocations"])
        invocation = report["invocations"][0]
        self.assertEqual("diagnose", invocation["skill"])
        self.assertEqual("explicit", invocation["classification"])
        self.assertEqual("high", invocation["classification_confidence"])
        self.assertEqual(["call-1", "call-2"], invocation["call_ids"])

    def test_confirms_current_autonomous_invocation_and_ignores_catalog(self) -> None:
        records = [
            record(
                "2026-08-19T10:29:58Z",
                "response_item",
                {
                    "type": "message",
                    "role": "developer",
                    "content": [
                        {
                            "type": "input_text",
                            "text": (
                                "Available skills: /tmp/.codex/skills/fake/SKILL.md\n"
                                "---\nname: fake\n"
                            ),
                        }
                    ],
                },
            ),
            record(
                "2026-08-19T10:30:00Z",
                "turn_context",
                {"turn_id": "turn-auto"},
            ),
            message("2026-08-19T10:30:01Z", "user", "Review this code for defects."),
            message(
                "2026-08-19T10:30:02Z",
                "assistant",
                "I’m using `code-audit` because this is a defect review.",
                turn_id="turn-auto",
            ),
            current_call(
                "2026-08-19T10:30:03Z",
                "turn-auto",
                "current-1",
                "sed -n '1,220p' /tmp/codex/skills/code-audit/SKILL.md",
            ),
            current_output(
                "2026-08-19T10:30:04Z",
                "current-1",
                "Script completed\nOutput:\n"
                "---\nname: code-audit\ndescription: fixture\n---\nbody\n",
            ),
        ]
        path = self.write_rollout("archived/session.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual(1, report["summary"]["confirmed_invocations"])
        invocation = report["invocations"][0]
        self.assertEqual("code-audit", invocation["skill"])
        self.assertEqual("autonomous", invocation["classification"])
        self.assertEqual("medium", invocation["classification_confidence"])
        self.assertNotIn("fake", json.dumps(report))

    def test_negated_assistant_mention_is_not_an_invocation_announcement(self) -> None:
        records = self.turn_records(
            turn_id="negated-announcement",
            user_text="Review this code without using code-audit.",
            assistant_text="I will not use code-audit for this review.",
            skill="code-audit",
            call_id="negated-announcement-call",
        )
        path = self.write_rollout("negated-announcement.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual(0, report["summary"]["confirmed_invocations"])
        self.assertEqual(1, report["summary"]["ambiguous_reads"])
        self.assertIn(
            "lacked a matching assistant invocation announcement",
            report["ambiguous_reads"][0]["reason"],
        )

    def test_negated_and_explanatory_user_mentions_are_not_explicit(self) -> None:
        prompts = (
            "Review this code without using code-audit.",
            "Do not use code-audit; inspect only skill-audit.",
            "Can you explain when to use code-audit?",
            "Describe using code-audit in historical sessions.",
            "Explain whether to use code-audit.",
            "Don't use $code-audit.",
            "Don’t use $code-audit.",
            "Never, under any circumstances, use code-audit.",
            "Under no circumstances, use code-audit.",
            "Do not, I repeat, use code-audit.",
        )
        for index, prompt in enumerate(prompts):
            with self.subTest(prompt=prompt):
                records = self.turn_records(
                    turn_id=f"negative-user-{index}",
                    user_text=prompt,
                    assistant_text="I am using code-audit for this review.",
                    skill="code-audit",
                    call_id=f"negative-user-call-{index}",
                )
                path = self.write_rollout(f"negative-user-{index}.jsonl", records)

                report = COLLECTOR.collect([path], self.cutoff)

                self.assertEqual(1, report["summary"]["confirmed_invocations"])
                self.assertEqual("unknown", report["invocations"][0]["classification"])

    def test_positive_negated_idioms_remain_explicit(self) -> None:
        prompts = (
            "Don't forget to use code-audit.",
            "Don't hesitate to use code-audit.",
            "Don't hesitate, use code-audit.",
        )
        for index, prompt in enumerate(prompts):
            with self.subTest(prompt=prompt):
                records = self.turn_records(
                    turn_id=f"positive-idiom-{index}",
                    user_text=prompt,
                    assistant_text="I am using code-audit now.",
                    skill="code-audit",
                    call_id=f"positive-idiom-call-{index}",
                )
                path = self.write_rollout(f"positive-idiom-{index}.jsonl", records)

                report = COLLECTOR.collect([path], self.cutoff)

                self.assertEqual("explicit", report["invocations"][0]["classification"])

    def test_recommendations_are_not_invocation_announcements(self) -> None:
        announcements = (
            "I recommend using code-audit in a later session.",
            "You can use code-audit later.",
            "The instructions say to use code-audit.",
        )
        for index, announcement in enumerate(announcements):
            with self.subTest(announcement=announcement):
                records = self.turn_records(
                    turn_id=f"recommendation-{index}",
                    user_text="Review this code.",
                    assistant_text=announcement,
                    skill="code-audit",
                    call_id=f"recommendation-call-{index}",
                )
                path = self.write_rollout(f"recommendation-{index}.jsonl", records)

                report = COLLECTOR.collect([path], self.cutoff)

                self.assertEqual(0, report["summary"]["confirmed_invocations"])

    def test_positive_request_after_negative_clause_remains_explicit(self) -> None:
        records = self.turn_records(
            turn_id="negative-then-request",
            user_text="No need to wait, use code-audit now.",
            assistant_text="I am using code-audit now.",
            skill="code-audit",
            call_id="negative-then-request-call",
        )
        path = self.write_rollout("negative-then-request.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual("explicit", report["invocations"][0]["classification"])

    def test_direct_request_question_remains_explicit(self) -> None:
        records = self.turn_records(
            turn_id="request-question",
            user_text="Can you use code-audit for this review?",
            assistant_text="I am using code-audit for this review.",
            skill="code-audit",
            call_id="request-question-call",
        )
        path = self.write_rollout("request-question.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual("explicit", report["invocations"][0]["classification"])

    def test_include_all_finds_system_skill_paths(self) -> None:
        records = self.turn_records(
            turn_id="system-skill",
            user_text="Use $skill-creator for this task.",
            assistant_text="I am using skill-creator for this task.",
            skill="skill-creator",
            call_id="system-skill-call",
        )
        call = next(
            item
            for item in records
            if item.get("payload", {}).get("type") == "function_call"
        )
        arguments = json.loads(call["payload"]["arguments"])
        arguments["cmd"] = arguments["cmd"].replace(
            "/skills/skill-creator/", "/skills/.system/skill-creator/"
        )
        call["payload"]["arguments"] = json.dumps(arguments)
        path = self.write_rollout("system-skill.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff, allowed_skills=None)

        self.assertEqual(1, report["summary"]["confirmed_invocations"])
        self.assertEqual(
            ["skills/.system/skill-creator/SKILL.md"],
            report["invocations"][0]["evidence_paths"],
        )

    def test_event_user_message_excludes_injected_role_user_marker(self) -> None:
        records = [
            record(
                "2026-08-19T10:35:00Z",
                "turn_context",
                {"turn_id": "event-turn"},
            ),
            message(
                "2026-08-19T10:35:01Z",
                "user",
                "# AGENTS.md\nAlways use /diagnose for incidents.",
            ),
            record(
                "2026-08-19T10:35:02Z",
                "event_msg",
                {"type": "user_message", "message": "Find the cause of this incident."},
            ),
            message(
                "2026-08-19T10:35:03Z",
                "assistant",
                "I am using diagnose to investigate the incident.",
                turn_id="event-turn",
            ),
            legacy_call(
                "2026-08-19T10:35:04Z",
                "event-turn",
                "event-call",
                "cat /tmp/.codex/skills/diagnose/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T10:35:05Z",
                "event-call",
                "---\nname: diagnose\ndescription: fixture\n---\nbody\n",
            ),
        ]
        path = self.write_rollout("event-user.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        invocation = report["invocations"][0]
        self.assertEqual("autonomous", invocation["classification"])
        self.assertEqual("medium", invocation["classification_confidence"])

    def test_plugin_qualification_uses_announced_canonical_name(self) -> None:
        records = [
            record(
                "2026-08-19T11:00:00Z",
                "turn_context",
                {"turn_id": "plugin-turn"},
            ),
            message("2026-08-19T11:00:01Z", "user", "Help with this repository."),
            message(
                "2026-08-19T11:00:02Z",
                "assistant",
                "I will use `github:github` to inspect it.",
                turn_id="plugin-turn",
            ),
            legacy_call(
                "2026-08-19T11:00:03Z",
                "plugin-turn",
                "plugin-call",
                "sed -n '1,200p' "
                "/tmp/plugins/cache/openai-curated/github/hash/skills/github/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T11:00:04Z",
                "plugin-call",
                "---\nname: github\ndescription: fixture\n---\nbody\n",
            ),
        ]
        path = self.write_rollout("plugin.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual("github:github", report["invocations"][0]["skill"])

    def test_successful_unannounced_read_is_ambiguous_not_counted(self) -> None:
        records = [
            record(
                "2026-08-19T10:40:00Z",
                "turn_context",
                {"turn_id": "audit-turn"},
            ),
            message("2026-08-19T10:40:01Z", "user", "Audit the target skill."),
            legacy_call(
                "2026-08-19T10:40:02Z",
                "audit-turn",
                "inspect-target",
                "cat /tmp/.codex/skills/target/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T10:40:03Z",
                "inspect-target",
                "---\nname: target\ndescription: fixture\n---\nbody\n",
            ),
        ]
        path = self.write_rollout("audit.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual(0, report["summary"]["confirmed_invocations"])
        self.assertEqual(1, report["summary"]["ambiguous_reads"])
        ambiguous = report["ambiguous_reads"][0]
        self.assertEqual("medium", ambiguous["confidence"])
        self.assertIn("lacked a matching assistant", ambiguous["reason"])

    def test_failed_and_truncated_reads_report_distinct_ambiguity(self) -> None:
        records = [
            record(
                "2026-08-19T10:45:00Z",
                "turn_context",
                {"turn_id": "broken-turn"},
            ),
            message("2026-08-19T10:45:01Z", "user", "Use the broken skill."),
            message(
                "2026-08-19T10:45:02Z",
                "assistant",
                "I will use the broken skill.",
                turn_id="broken-turn",
            ),
            legacy_call(
                "2026-08-19T10:45:03Z",
                "broken-turn",
                "failed",
                "cat /tmp/.codex/skills/broken/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T10:45:04Z",
                "failed",
                "Process exited with code 1\nNo such file\n",
            ),
            legacy_call(
                "2026-08-19T10:45:05Z",
                "broken-turn",
                "truncated",
                "sed -n '1,20p' /tmp/.codex/skills/truncated/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T10:45:06Z",
                "truncated",
                "Process exited with code 0\nWarning: output truncated\n",
            ),
        ]
        path = self.write_rollout("ambiguous.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        reasons = {item["skill"]: item for item in report["ambiguous_reads"]}
        self.assertEqual("low", reasons["broken"]["confidence"])
        self.assertIn("nonzero", reasons["broken"]["reason"])
        self.assertEqual("medium", reasons["truncated"]["confidence"])
        self.assertIn("frontmatter", reasons["truncated"]["reason"])

    def test_cutoff_is_exclusive_and_prevents_self_contamination(self) -> None:
        records = self.turn_records(
            turn_id="before",
            user_text="Use the verify skill.",
            assistant_text="I am using the verify skill.",
            skill="verify",
            call_id="before-call",
        )
        at_cutoff = self.turn_records(
            turn_id="audit-self",
            user_text="Run /skill-audit.",
            assistant_text="I am using the skill-audit skill.",
            skill="skill-audit",
            call_id="self-call",
        )
        for item in at_cutoff:
            item["timestamp"] = "2026-08-19T12:00:00Z"
        path = self.write_rollout("cutoff.jsonl", records + at_cutoff)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual(["verify"], [item["skill"] for item in report["invocations"]])
        self.assertNotIn("skill-audit", json.dumps(report["invocations"]))

    def test_agent_skill_cat_is_a_supported_read(self) -> None:
        records = [
            record(
                "2026-08-19T10:50:00Z",
                "turn_context",
                {"turn_id": "resolver-turn"},
            ),
            message("2026-08-19T10:50:01Z", "user", "Use $verify."),
            message(
                "2026-08-19T10:50:02Z",
                "assistant",
                "I’m invoking `verify` for this criteria check.",
                turn_id="resolver-turn",
            ),
            legacy_call(
                "2026-08-19T10:50:03Z",
                "resolver-turn",
                "resolver-call",
                "/tmp/bin/agent-skill cat verify --tool codex",
            ),
            legacy_output(
                "2026-08-19T10:50:04Z",
                "resolver-call",
                "---\nname: verify\ndescription: fixture\n---\nbody\n",
            ),
        ]
        path = self.write_rollout("resolver.jsonl", records)

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual(1, report["summary"]["confirmed_invocations"])
        self.assertEqual("explicit", report["invocations"][0]["classification"])

    def test_scope_ignores_non_catalog_skills_and_reports_coverage(self) -> None:
        records = [
            record(
                "2026-08-19T10:00:00Z",
                "turn_context",
                {"turn_id": "scope-turn"},
            ),
            message("2026-08-19T10:00:01Z", "user", "Review this repository."),
            message(
                "2026-08-19T10:00:02Z",
                "assistant",
                "I am using `github:github` for repository context.",
                turn_id="scope-turn",
            ),
            legacy_call(
                "2026-08-19T10:00:03Z",
                "scope-turn",
                "scope-call",
                "cat /tmp/plugins/cache/openai-curated/github/hash/skills/"
                "github/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T10:00:04Z",
                "scope-call",
                "---\nname: github\ndescription: fixture\n---\nbody\n",
            ),
        ]
        path = self.write_rollout("scope.jsonl", records)

        report = COLLECTOR.collect(
            [path],
            self.cutoff,
            allowed_skills={"github"},
            scope_label="fixture personal catalog",
        )

        self.assertEqual(0, report["summary"]["confirmed_invocations"])
        self.assertEqual(1, report["summary"]["candidate_reads_seen"])
        self.assertEqual(0, report["summary"]["eligible_candidate_reads"])
        self.assertEqual(1, report["summary"]["ignored_candidate_reads"])
        self.assertEqual(["github:github"], report["scope"]["ignored_skills"])

    def test_combined_read_confirms_only_candidate_specific_announcement(self) -> None:
        records = [
            record(
                "2026-08-19T10:10:00Z",
                "turn_context",
                {"turn_id": "combined-turn"},
            ),
            message("2026-08-19T10:10:01Z", "user", "Review this code."),
            message(
                "2026-08-19T10:10:02Z",
                "assistant",
                "I am using `code-audit` for the defect review.",
                turn_id="combined-turn",
            ),
            legacy_call(
                "2026-08-19T10:10:03Z",
                "combined-turn",
                "combined-call",
                "cat /tmp/.codex/skills/code-audit/SKILL.md "
                "/tmp/.codex/skills/noncatalog/SKILL.md",
            ),
            legacy_output(
                "2026-08-19T10:10:04Z",
                "combined-call",
                "---\nname: code-audit\ndescription: fixture\n---\nbody\n"
                "---\nname: noncatalog\ndescription: fixture\n---\nbody\n",
            ),
        ]
        path = self.write_rollout("combined.jsonl", records)

        report = COLLECTOR.collect(
            [path],
            self.cutoff,
            allowed_skills={"code-audit"},
            scope_label="fixture personal catalog",
        )

        self.assertEqual(
            ["code-audit"], [item["skill"] for item in report["invocations"]]
        )
        self.assertEqual(2, report["summary"]["candidate_reads_seen"])
        self.assertEqual(1, report["summary"]["eligible_candidate_reads"])
        self.assertEqual(1, report["summary"]["ignored_candidate_reads"])
        self.assertEqual(["noncatalog"], report["scope"]["ignored_skills"])

    def test_directory_inputs_deduplicate_same_turn_across_session_ids(self) -> None:
        records = [
            record(
                "2026-08-19T09:59:59Z",
                "session_meta",
                {"id": "stable-session"},
            ),
            *self.turn_records(
                turn_id="same-turn",
                user_text="Use /diagnose.",
                assistant_text="I’m using diagnose for this incident.",
                skill="diagnose",
                call_id="same-call",
            ),
        ]
        resumed_records = json.loads(json.dumps(records))
        resumed_records[0]["payload"]["id"] = "resumed-session"
        resumed_records = [
            item
            for item in resumed_records
            if not (
                item.get("type") == "response_item"
                and item.get("payload", {}).get("role") == "user"
            )
        ]
        self.write_rollout("sessions/copy.jsonl", records)
        self.write_rollout("archived/copy.jsonl", resumed_records)

        report = COLLECTOR.collect(
            [self.root / "sessions", self.root / "archived"], self.cutoff
        )

        self.assertEqual(2, report["summary"]["rollout_files_scanned"])
        self.assertEqual(1, report["summary"]["confirmed_invocations"])
        self.assertEqual(
            ["resumed-session", "stable-session"],
            report["invocations"][0]["session_ids"],
        )
        self.assertEqual("explicit", report["invocations"][0]["classification"])

    def test_cli_json_and_human_outputs_are_deterministic(self) -> None:
        path = self.write_rollout(
            "cli.jsonl",
            self.turn_records(
                turn_id="cli-turn",
                user_text="Use /diagnose.",
                assistant_text="I’m using diagnose.",
                skill="diagnose",
                call_id="cli-call",
            ),
        )
        base_command = [
            sys.executable,
            str(SCRIPT),
            "--cutoff",
            "2026-08-19T12:00:00Z",
            "--input",
            str(path),
        ]

        first = subprocess.run(
            [*base_command, "--format", "json"],
            check=True,
            capture_output=True,
            text=True,
        )
        second = subprocess.run(
            [*base_command, "--format", "json"],
            check=True,
            capture_output=True,
            text=True,
        )
        human = subprocess.run(
            [*base_command, "--format", "human"],
            check=True,
            capture_output=True,
            text=True,
        )

        self.assertEqual(first.stdout, second.stdout)
        payload = json.loads(first.stdout)
        self.assertEqual(1, payload["summary"]["confirmed_invocations"])
        self.assertIn("diagnose: 1 (explicit 1, autonomous 0, unknown 0)", human.stdout)
        self.assertEqual("", first.stderr)
        self.assertEqual("", human.stderr)

    def test_malformed_and_missing_timestamp_records_are_reported(self) -> None:
        path = self.root / "diagnostics.jsonl"
        path.write_text(
            "not-json\n"
            + json.dumps({"type": "response_item", "payload": {}})
            + "\n"
            + json.dumps(
                legacy_call(
                    "2026-08-19T10:00:00Z",
                    "diagnostic-turn",
                    "diagnostic-call",
                    "cat /tmp/.codex/skills/diagnostic/SKILL.md",
                ),
                sort_keys=True,
            )
            + "\n",
            encoding="utf-8",
        )

        report = COLLECTOR.collect([path], self.cutoff)

        self.assertEqual(1, report["summary"]["malformed_lines"])
        self.assertEqual(1, report["summary"]["records_without_timestamps"])

    def test_cli_catches_missing_ripgrep_without_traceback(self) -> None:
        path = self.write_rollout(
            "missing-rg.jsonl",
            self.turn_records(
                turn_id="missing-rg-turn",
                user_text="Use /diagnose.",
                assistant_text="I am using diagnose.",
                skill="diagnose",
                call_id="missing-rg-call",
            ),
        )
        stderr = io.StringIO()

        with mock.patch.object(COLLECTOR.shutil, "which", return_value=None):
            with redirect_stderr(stderr), self.assertRaises(SystemExit) as raised:
                COLLECTOR.main(
                    [
                        "--cutoff",
                        "2026-08-19T12:00:00Z",
                        "--input",
                        str(path),
                        "--include-all",
                    ]
                )

        self.assertEqual(2, raised.exception.code)
        self.assertIn("ripgrep (rg) is required", stderr.getvalue())
        self.assertNotIn("Traceback", stderr.getvalue())

    def test_cli_catches_missing_input_without_traceback(self) -> None:
        stderr = io.StringIO()

        with redirect_stderr(stderr), self.assertRaises(SystemExit) as raised:
            COLLECTOR.main(
                [
                    "--cutoff",
                    "2026-08-19T12:00:00Z",
                    "--input",
                    str(self.root / "missing.jsonl"),
                    "--include-all",
                ]
            )

        self.assertEqual(2, raised.exception.code)
        self.assertIn("rollout input does not exist", stderr.getvalue())
        self.assertNotIn("Traceback", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()
