import importlib.machinery
import importlib.util
import json
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "cr-taiga"
RUN_WRAPPER = ROOT / "bin" / "cr-taiga-run"
COMPARE_WRAPPER = ROOT / "bin" / "cr-taiga-compare"


def load_module():
    loader = importlib.machinery.SourceFileLoader("cr_taiga", str(SCRIPT))
    spec = importlib.util.spec_from_loader("cr_taiga", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["cr_taiga"] = module
    spec.loader.exec_module(module)
    return module


def write_json(path, payload):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload), encoding="utf-8")


def reward(score, subscores):
    return {
        "normalized_score": score,
        "is_auxiliary_grade": False,
        "subscores": [
            {
                "grading_criteria": criterion,
                "score": value,
                "max_score": 1.0,
                "reasoning": "",
            }
            for criterion, value in subscores.items()
        ],
    }


def write_run(task_dir, label, attempt, job_id, score, subscores):
    write_json(
        task_dir / "taiga" / label / str(attempt) / "run.json",
        {
            "id": f"run-{job_id}-{attempt}",
            "job_id": job_id,
            "job_name": f"grantmaking-{task_dir.name}",
            "problem_id": task_dir.name,
            "attempt_number": attempt,
            "created_at": "2026-06-30T21:44:45Z",
            "final_score": score,
            "rewards": [reward(score, subscores)],
        },
    )


class CrTaigaTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def make_task(self, temp):
        worktree = pathlib.Path(temp) / "worktree"
        task_dir = worktree / "tasks" / "sample-task"
        write_json(task_dir / "basic_info.json", {"id": "sample-task"})
        (task_dir / "prompt.txt").write_text("Prompt\n", encoding="utf-8")
        (task_dir / "grading").mkdir(parents=True, exist_ok=True)
        (task_dir / "grading" / "rubric.md").write_text("Rubric\n", encoding="utf-8")
        return worktree, task_dir

    def test_build_ledger_event_binds_revision_image_job_and_hashes(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self.make_task(temp)
            git_info = self.mod.GitInfo(
                sha="abc123",
                branch="pablo/sample-task",
                is_dirty=False,
            )

            event = self.mod.build_ledger_event(
                task_dir=task_dir,
                git_info=git_info,
                status="submitted",
                model="claude-opus-4-8",
                epochs=10,
                job_id="11111111-1111-4111-8111-111111111111",
                build_run_url="https://github.com/org/repo/actions/runs/123",
                note="after hardening",
            )

        self.assertEqual(event["slug"], "sample-task")
        self.assertEqual(event["git_sha"], "abc123")
        self.assertEqual(event["branch"], "pablo/sample-task")
        self.assertEqual(
            event["image"],
            "us-east1-docker.pkg.dev/gcp-taiga/trajectorylabs/grantmaking:sha-abc123",
        )
        self.assertEqual(event["job_id"], "11111111-1111-4111-8111-111111111111")
        self.assertIn("transcripts/?id=11111111-1111-4111-8111-111111111111", event["transcript_url"])
        self.assertEqual(set(event["task_hashes"]), {"basic_info.json", "prompt.txt", "grading/rubric.md"})
        self.assertEqual(event["note"], "after hardening")

    def test_latest_and_previous_resolve_from_ledger(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            ledger = task_dir / "taiga" / "run-ledger.jsonl"
            events = [
                {"status": "submitted", "job_id": "old-job", "recorded_at": "2026-06-30T10:00:00Z"},
                {"status": "planned", "recorded_at": "2026-06-30T11:00:00Z"},
                {"status": "submitted", "job_id": "new-job", "recorded_at": "2026-06-30T12:00:00Z"},
            ]
            ledger.parent.mkdir(parents=True, exist_ok=True)
            ledger.write_text("\n".join(json.dumps(event) for event in events) + "\n", encoding="utf-8")

            self.assertEqual(self.mod.resolve_job_spec(task_dir, "latest"), "new-job")
            self.assertEqual(self.mod.resolve_job_spec(task_dir, "previous"), "old-job")

    def test_trajectory_tasks_use_private_parent_ledger(self):
        with tempfile.TemporaryDirectory() as temp:
            trajectory = pathlib.Path(temp) / "Trajectory"
            (trajectory / "agent-c").mkdir(parents=True)
            (trajectory / "cr-pipeline").mkdir()
            task_dir = trajectory / "agent-c" / "sample-worktree" / "tasks" / "sample-task"
            task_dir.mkdir(parents=True)

            path = self.mod.ledger_path(task_dir)

        self.assertEqual(
            path,
            trajectory / "logs" / "taiga-ledger" / "sample-task.jsonl",
        )

    def test_compare_exact_jobs_outputs_score_and_criterion_deltas(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            write_run(task_dir, "grantmaking-sample-task", 1, "old-job", 0.50, {"a": 0.0, "b": 1.0})
            write_run(task_dir, "grantmaking-sample-task", 2, "old-job", 1.00, {"a": 1.0, "b": 1.0})
            write_run(task_dir, "grantmaking-sample-task-new", 1, "new-job", 0.75, {"a": 0.5, "b": 1.0})
            write_run(task_dir, "grantmaking-sample-task-new", 2, "new-job", 0.25, {"a": 0.5, "b": 0.0})

            report = self.mod.compare_jobs(task_dir, "old-job", "new-job")

        self.assertIn("| Overall | 0.750 | 0.500 | -0.250 |", report)
        self.assertIn("| a | 0.500 | 0.500 | +0.000 |", report)
        self.assertIn("| b | 1.000 | 0.500 | -0.500 |", report)

    def test_archive_preserves_job_for_later_comparison(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            write_run(task_dir, "grantmaking-sample-task", 1, "old-job", 0.50, {"a": 0.0})
            write_run(task_dir, "grantmaking-sample-task", 2, "old-job", 1.00, {"a": 1.0})

            archive = self.mod.archive_job_artifacts(task_dir, "old-job")
            shutil.rmtree(task_dir / "taiga" / "grantmaking-sample-task")
            summary = self.mod.summarize_job(task_dir, "old-job")

        self.assertEqual(archive.name, "old-job")
        self.assertEqual(summary["n"], 2)
        self.assertEqual(summary["mean"], 0.75)

    def test_artifact_directory_without_job_id_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            write_json(task_dir / "taiga" / "transcoded" / "attempt-01" / "run.json", {"id": "run-1", "attempt": 1})

            with self.assertRaisesRegex(self.mod.TaigaLedgerError, "does not contain a Taiga job_id"):
                self.mod.resolve_job_spec(task_dir, "transcoded")

    def test_path_wrappers_forward_to_main_subcommands(self):
        for wrapper, subcommand in [
            (RUN_WRAPPER, "run"),
            (COMPARE_WRAPPER, "compare"),
        ]:
            result = subprocess.run(
                [str(wrapper), "--print-forwarded-command", "sample-task"],
                text=True,
                capture_output=True,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            command = json.loads(result.stdout)
            self.assertEqual(pathlib.Path(command[0]).name, "cr-taiga")
            self.assertEqual(command[1], subcommand)


if __name__ == "__main__":
    unittest.main()
