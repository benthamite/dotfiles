import importlib.machinery
import importlib.util
import json
import pathlib
import shutil
import subprocess
import sys
import tempfile
import textwrap
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


def git(worktree, *args):
    return subprocess.run(
        ["git", "-C", str(worktree), *args],
        text=True,
        capture_output=True,
        check=True,
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

    def make_git_task(self, temp):
        worktree, task_dir = self.make_task(temp)
        git(worktree, "init")
        git(worktree, "config", "user.email", "test@example.com")
        git(worktree, "config", "user.name", "Test User")
        git(worktree, "add", ".")
        git(worktree, "commit", "-m", "baseline")
        return worktree, task_dir

    def write_impact_report(
        self,
        task_dir,
        task_tree_hash,
        recommendation="run",
        weight_change_justifications=None,
    ):
        report = task_dir / "grading" / "hardening-impact.md"
        meta = {
            "schema_version": 1,
            "slug": task_dir.name,
            "task_tree_hash": task_tree_hash,
            "baseline_job_id": "old-job",
            "baseline_mean": 0.678,
            "estimated_mean": 0.61,
            "estimated_delta": -0.068,
            "recommendation": recommendation,
            "reviewed_prompt_risks": True,
        }
        if weight_change_justifications is not None:
            meta["weight_change_justifications"] = weight_change_justifications
        report.write_text(
            "# Hardening impact\n\n<!-- cr-taiga-impact\n"
            + json.dumps(meta, indent=2)
            + "\n-->\n\n## Cross-criterion effects\n\n"
            "Reviewed all prompt edits against every criterion.\n",
            encoding="utf-8",
        )
        return report

    def _rubric(self, crit_a_pts, crit_b_pts, crit_a_tail=""):
        return textwrap.dedent(
            f"""\
            | id | Criterion | Pts | Pass condition |
            | -- | --------- | --- | -------------- |
            | `crit_a` | **A** | {crit_a_pts} | Does A.{crit_a_tail} |
            | `crit_b` | **B** | {crit_b_pts} | Does B. |
            """
        )

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
        self.assertIn(
            "transcripts/?id=11111111-1111-4111-8111-111111111111",
            event["transcript_url"],
        )
        self.assertEqual(
            set(event["task_hashes"]),
            {"basic_info.json", "prompt.txt", "grading/rubric.md"},
        )
        self.assertEqual(event["note"], "after hardening")

    def test_latest_and_previous_resolve_from_ledger(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            ledger = task_dir / "taiga" / "run-ledger.jsonl"
            events = [
                {
                    "status": "submitted",
                    "job_id": "old-job",
                    "recorded_at": "2026-06-30T10:00:00Z",
                },
                {"status": "planned", "recorded_at": "2026-06-30T11:00:00Z"},
                {
                    "status": "submitted",
                    "job_id": "new-job",
                    "recorded_at": "2026-06-30T12:00:00Z",
                },
            ]
            ledger.parent.mkdir(parents=True, exist_ok=True)
            ledger.write_text(
                "\n".join(json.dumps(event) for event in events) + "\n",
                encoding="utf-8",
            )

            self.assertEqual(self.mod.resolve_job_spec(task_dir, "latest"), "new-job")
            self.assertEqual(self.mod.resolve_job_spec(task_dir, "previous"), "old-job")

    def test_trajectory_tasks_use_private_parent_ledger(self):
        with tempfile.TemporaryDirectory() as temp:
            trajectory = pathlib.Path(temp) / "Trajectory"
            (trajectory / "reasoning-tasks").mkdir(parents=True)
            (trajectory / "cr-pipeline").mkdir()
            task_dir = (
                trajectory / "reasoning-tasks" / "sample-worktree" / "tasks" / "sample-task"
            )
            task_dir.mkdir(parents=True)

            path = self.mod.ledger_path(task_dir)

        self.assertEqual(
            path,
            trajectory / "logs" / "taiga-ledger" / "sample-task.jsonl",
        )

    def test_compare_exact_jobs_outputs_score_and_criterion_deltas(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            write_run(
                task_dir,
                "grantmaking-sample-task",
                1,
                "old-job",
                0.50,
                {"a": 0.0, "b": 1.0},
            )
            write_run(
                task_dir,
                "grantmaking-sample-task",
                2,
                "old-job",
                1.00,
                {"a": 1.0, "b": 1.0},
            )
            write_run(
                task_dir,
                "grantmaking-sample-task-new",
                1,
                "new-job",
                0.75,
                {"a": 0.5, "b": 1.0},
            )
            write_run(
                task_dir,
                "grantmaking-sample-task-new",
                2,
                "new-job",
                0.25,
                {"a": 0.5, "b": 0.0},
            )

            report = self.mod.compare_jobs(task_dir, "old-job", "new-job")

        self.assertIn("| Overall | 0.750 | 0.500 | -0.250 |", report)
        self.assertIn("| a | 0.500 | 0.500 | +0.000 |", report)
        self.assertIn("| b | 1.000 | 0.500 | -0.500 |", report)

    def test_archive_preserves_job_for_later_comparison(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            write_run(
                task_dir, "grantmaking-sample-task", 1, "old-job", 0.50, {"a": 0.0}
            )
            write_run(
                task_dir, "grantmaking-sample-task", 2, "old-job", 1.00, {"a": 1.0}
            )

            archive = self.mod.archive_job_artifacts(task_dir, "old-job")
            shutil.rmtree(task_dir / "taiga" / "grantmaking-sample-task")
            summary = self.mod.summarize_job(task_dir, "old-job")

        self.assertEqual(archive.name, "old-job")
        self.assertEqual(summary["n"], 2)
        self.assertEqual(summary["mean"], 0.75)

    def test_artifact_directory_without_job_id_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            write_json(
                task_dir / "taiga" / "transcoded" / "attempt-01" / "run.json",
                {"id": "run-1", "attempt": 1},
            )

            with self.assertRaisesRegex(
                self.mod.TaigaLedgerError, "does not contain a Taiga job_id"
            ):
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

    def test_preflight_fails_when_task_changed_without_impact_report(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self.make_git_task(temp)
            (task_dir / "prompt.txt").write_text(
                "Prompt with new hardening cue\n", encoding="utf-8"
            )

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "preflight",
                    "sample-task",
                    "--worktree",
                    str(worktree),
                    "--base-ref",
                    "HEAD",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 2)
        self.assertIn("impact report required", result.stderr)
        self.assertIn("prompt.txt", result.stderr)

    def test_preflight_flags_prompt_phrases_that_mirror_rubric(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self.make_git_task(temp)
            (task_dir / "grading" / "rubric.md").write_text(
                textwrap.dedent(
                    """\
                    | id | Criterion | Pts | Pass condition |
                    | -- | --------- | --- | -------------- |
                    | `trade_bloc_threshold` | **Trade makes the bloc, not the country, the growth unit; self-sufficiency is costly but potentially bearable** | 15 | A lone country cannot pull away because the rest of the world can trade with itself and outgrow it. Also explains whether decoupling costs are fatal or potentially bearable. |
                    """
                ),
                encoding="utf-8",
            )
            git(worktree, "add", ".")
            git(worktree, "commit", "-m", "rubric")
            (task_dir / "prompt.txt").write_text(
                "Q1: explain whether those costs are fatal or potentially bearable, and whether the unit is outgrown by the rest of the world trading with itself.\n",
                encoding="utf-8",
            )

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "preflight",
                    "sample-task",
                    "--worktree",
                    str(worktree),
                    "--base-ref",
                    "HEAD",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 2)
        self.assertIn("prompt over-scaffolding risk", result.stderr)
        self.assertIn("fatal or potentially bearable", result.stderr)
        self.assertIn("outgrown by the rest of the world", result.stderr)

    def test_preflight_accepts_current_impact_report(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self.make_git_task(temp)
            (task_dir / "prompt.txt").write_text(
                "Prompt with new hardening cue\n", encoding="utf-8"
            )
            task_tree = self.mod.task_tree_hash(self.mod.task_hashes(task_dir))
            self.write_impact_report(task_dir, task_tree)

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "preflight",
                    "sample-task",
                    "--worktree",
                    str(worktree),
                    "--base-ref",
                    "HEAD",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("preflight passed", result.stdout)

    def test_preflight_rejects_stale_impact_report(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self.make_git_task(temp)
            (task_dir / "prompt.txt").write_text(
                "Prompt with new hardening cue\n", encoding="utf-8"
            )
            self.write_impact_report(task_dir, "stale-hash")

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "preflight",
                    "sample-task",
                    "--worktree",
                    str(worktree),
                    "--base-ref",
                    "HEAD",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 2)
        self.assertIn("stale impact report", result.stderr)

    def test_compare_jobs_leads_with_regression_alarm_for_large_score_jump(self):
        with tempfile.TemporaryDirectory() as temp:
            _, task_dir = self.make_task(temp)
            write_run(
                task_dir, "grantmaking-sample-task-old", 1, "old-job", 0.60, {"a": 0.5}
            )
            write_run(
                task_dir, "grantmaking-sample-task-new", 1, "new-job", 0.95, {"a": 1.0}
            )

            report = self.mod.compare_jobs(task_dir, "old-job", "new-job")

        self.assertTrue(report.startswith("REGRESSION ALARM"))
        self.assertIn("Overall changed by +0.350", report)
        self.assertIn("a changed by +0.500", report)

    def _commit_v1_then_stage_working(self, temp, working_rubric):
        worktree, task_dir = self.make_git_task(temp)
        (task_dir / "grading" / "rubric.md").write_text(
            self._rubric(13, 9), encoding="utf-8"
        )
        git(worktree, "add", ".")
        git(worktree, "commit", "-m", "v1 rubric")
        (task_dir / "grading" / "rubric.md").write_text(
            working_rubric, encoding="utf-8"
        )
        return worktree, task_dir

    def _run_preflight(self, worktree):
        return subprocess.run(
            [
                str(SCRIPT),
                "preflight",
                "sample-task",
                "--worktree",
                str(worktree),
                "--base-ref",
                "HEAD",
            ],
            text=True,
            capture_output=True,
        )

    def test_preflight_blocks_reweight_without_importance_justification(self):
        with tempfile.TemporaryDirectory() as temp:
            # crit_a 13->9, crit_b 9->13: a pure reweight, no pass-condition change.
            worktree, task_dir = self._commit_v1_then_stage_working(
                temp, self._rubric(9, 13)
            )
            task_tree = self.mod.task_tree_hash(self.mod.task_hashes(task_dir))
            self.write_impact_report(
                task_dir, task_tree
            )  # no weight_change_justifications

            result = self._run_preflight(worktree)

        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertIn("weights changed", result.stderr)
        self.assertIn("importance", result.stderr)
        self.assertIn("crit_a", result.stderr)
        self.assertIn("crit_b", result.stderr)

    def test_preflight_accepts_reweight_with_importance_justification(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self._commit_v1_then_stage_working(
                temp, self._rubric(9, 13)
            )
            task_tree = self.mod.task_tree_hash(self.mod.task_hashes(task_dir))
            self.write_impact_report(
                task_dir,
                task_tree,
                weight_change_justifications={
                    "crit_a": "A is a textbook step; lower intrinsic importance than B.",
                    "crit_b": "B is the load-bearing discriminator; was underweighted.",
                },
            )

            result = self._run_preflight(worktree)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("preflight passed", result.stdout)

    def test_preflight_rejects_reweight_with_partial_justification(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self._commit_v1_then_stage_working(
                temp, self._rubric(9, 13)
            )
            task_tree = self.mod.task_tree_hash(self.mod.task_hashes(task_dir))
            self.write_impact_report(
                task_dir,
                task_tree,
                weight_change_justifications={
                    "crit_a": "textbook step"
                },  # crit_b missing
            )

            result = self._run_preflight(worktree)

        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertIn("without an importance justification", result.stderr)
        self.assertIn("crit_b", result.stderr)

    def test_preflight_allows_passcondition_change_without_weight_change(self):
        with tempfile.TemporaryDirectory() as temp:
            # Same weights (13, 9) but crit_a's pass condition tightened: no weight gate.
            worktree, task_dir = self._commit_v1_then_stage_working(
                temp, self._rubric(13, 9, crit_a_tail=" Also does A-prime.")
            )
            task_tree = self.mod.task_tree_hash(self.mod.task_hashes(task_dir))
            self.write_impact_report(task_dir, task_tree)  # no justifications needed

            result = self._run_preflight(worktree)

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("preflight passed", result.stdout)

    def test_criterion_weights_and_changes_parse(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self._commit_v1_then_stage_working(
                temp, self._rubric(9, 13)
            )
            weights = self.mod.criterion_weights_from_text(self._rubric(13, 9))
            self.assertEqual(weights, {"crit_a": 13.0, "crit_b": 9.0})
            # resolve() so task_dir and git's toplevel agree on macOS (/var vs /private/var);
            # real worktrees live under /Users and never hit this symlink mismatch.
            changes = self.mod.rubric_weight_changes(task_dir.resolve(), "HEAD")
            self.assertEqual(changes, {"crit_a": (13.0, 9.0), "crit_b": (9.0, 13.0)})

    def test_run_blocks_hardening_rerun_without_base_ref(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self.make_git_task(temp)
            # A prior submitted run recorded against the v1 rubric.
            (task_dir / "grading" / "rubric.md").write_text(
                self._rubric(13, 9), encoding="utf-8"
            )
            v1_hashes = self.mod.task_hashes(task_dir)
            ledger = task_dir / "taiga" / "run-ledger.jsonl"
            ledger.parent.mkdir(parents=True, exist_ok=True)
            ledger.write_text(
                json.dumps(
                    {
                        "status": "submitted",
                        "job_id": "old-job",
                        "task_hashes": v1_hashes,
                        "recorded_at": "2026-06-30T10:00:00Z",
                    }
                )
                + "\n",
                encoding="utf-8",
            )
            # Rubric now reweighted (v2); try to record a submit WITHOUT --base-ref.
            (task_dir / "grading" / "rubric.md").write_text(
                self._rubric(9, 13), encoding="utf-8"
            )

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "run",
                    "sample-task",
                    "--worktree",
                    str(worktree),
                    "--status",
                    "submitted",
                    "--job-id",
                    "new-job",
                    "--no-write",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 2, result.stdout + result.stderr)
        self.assertIn("no --base-ref", result.stderr)
        self.assertIn("grading/rubric.md", result.stderr)

    def test_run_allows_rerun_with_base_ref_and_valid_report(self):
        with tempfile.TemporaryDirectory() as temp:
            worktree, task_dir = self.make_git_task(temp)
            (task_dir / "grading" / "rubric.md").write_text(
                self._rubric(13, 9), encoding="utf-8"
            )
            git(worktree, "add", ".")
            git(worktree, "commit", "-m", "v1")
            # Pass-condition tightening only (no weight change) vs HEAD.
            (task_dir / "grading" / "rubric.md").write_text(
                self._rubric(13, 9, crit_a_tail=" Also A-prime."), encoding="utf-8"
            )
            task_tree = self.mod.task_tree_hash(self.mod.task_hashes(task_dir))
            self.write_impact_report(task_dir, task_tree)

            result = subprocess.run(
                [
                    str(SCRIPT),
                    "run",
                    "sample-task",
                    "--worktree",
                    str(worktree),
                    "--status",
                    "planned",
                    "--base-ref",
                    "HEAD",
                    "--no-write",
                ],
                text=True,
                capture_output=True,
            )

        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
