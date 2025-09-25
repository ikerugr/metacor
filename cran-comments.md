## Test environments
- macOS 14, R 4.4.x (local): 0 ERRORs | 0 WARNINGs | 0 NOTEs
  (occasionally 1 NOTE: "unable to verify current time" on macOS; benign)
- Win-builder (R-release, R-devel): OK
- Ubuntu 22.04, R 4.4 (GH Actions): OK

## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new release.

## Changes in 1.2.0
- Redesigned `cv` imputation: robust global median from known SDdiff and
  feasible-interval estimates; prevents all-NA outcomes.
- Report writing uses `tempdir()` in non-interactive sessions.
