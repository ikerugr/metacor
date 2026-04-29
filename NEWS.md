# metacor 1.3.0

## Major changes

- **Naming overhaul.** Adopts a single canonical vocabulary across the entire
  package, paper, and documentation. The standard deviation of pre–post
  difference scores is now consistently called `sd_change` (was `sd_diff` in
  arguments and output columns; was sometimes `SDchange`, sometimes `SDdiff`
  in the manuscript). The control-group suffix becomes `_ctrl` (was `_Con`).
  Effect-size identifiers move to snake_case (`smd_pre`, `smd_change`,
  `smd_pooled`, `smd_diff_groups`). See `naming_decisions.md` for the full
  mapping.
- **Backwards compatibility layer.** All v1.2.x argument names and values
  continue to work but emit a `lifecycle::deprecate_warn()`. Slated for
  removal in **2.0.0**.
- **Flexible column matching.** A new argument
  `column_matching = c("flexible", "strict")` (default `"flexible"`) accepts
  user column names that differ from canonical only by formatting (`Mean.Pre`,
  `meanpreint`, `MeanPre_Treatment`, …) or that match a closed dictionary of
  documented synonyms. Every resolved match is reported. Ambiguity is a hard
  error; no Levenshtein / fuzzy matching is performed.

## Internal

- New regression / golden test suite under `tests/testthat/`. Locks down
  numeric output across every effect-size × imputation-method combination,
  using the published example dataset for single-group cases and a synthetic
  dataset for two-group cases.

---

# metacor 1.2.1

- Added `check_metacor_consistency()` to verify internal consistency of
  meta-analytic summary data (p-value reconstruction, CI reconstruction,
  feasibility of `sd_diff` given `sd_pre` and `sd_post`, and detection of
  extreme correlations). Optional `interpret = TRUE` adds a per-study
  narrative summary.

# metacor 1.2.0

- Redesigned the `cv` imputation method: missing `sd_diff` is now estimated
  from a robust **global median** of the coefficient of variation, computed
  from both fully observed studies and feasible-interval estimates derived
  from `sd_pre`/`sd_post`. Prevents the pathological all-NA outcomes that
  could occur in the previous implementation.
- The Word imputation report is now written to `tempdir()` in
  non-interactive sessions.

# metacor 1.1.2

- Bug fixes around `%change` calculation.

# metacor 1.1.1

- Fixes to imputation behaviour when input p-values were misreported.

# metacor 1.1.0

- First public release. `metacor_dual()` implements effect-size calculation
  (`SMDpre`, `SMDchange`, `ScMDpooled`, `ScMDpre`) and imputation of missing
  `sd_diff` and Pearson `r` (`none`, `direct`, `mean`, `cv`, manual) for
  pre–post designs with or without a control group, plus a Word imputation
  report.
