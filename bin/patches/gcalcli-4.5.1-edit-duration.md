# gcalcli 4.5.1 edit argument repair

The installed Homebrew gcalcli 4.5.1_13 calls `get_times_from_duration` with `all_day` in both interactive edit branches (start time and duration), while the function takes `allday`. Editing duration crashes before saving.

`gcalcli-4.5.1-edit-duration.patch` corrects the two callers. Applied locally on 2026-09-29 to `/opt/homebrew/Cellar/gcalcli/4.5.1_13/libexec/lib/python3.14/site-packages/gcalcli/gcal.py`. This is a direct source correction, not a runtime fallback. A Homebrew reinstall may replace it; inspect the installed callers and utility signature before applying it to any other build. The patch is relative to the Python site-packages directory. No automatic patching or upstream publication is installed.
