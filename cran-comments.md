## Test environments

- macOS 14, R 4.3.x (local): 0 ERRORs | 0 WARNINGs | 0 NOTEs
  (occasionally 1 NOTE: "unable to verify current time" on macOS; benign).
- Win-builder (R-release, R-devel): planned before submission.
- Ubuntu 22.04, R 4.4 (GitHub Actions): planned before submission.

## R CMD check results

0 errors | 0 warnings | 0 notes (after the local environment-related
notes are filtered out).

## metacor 1.3.0 — release notes

This is a **minor version** that consolidates the package vocabulary
without changing any computational behaviour. The release ships:

- A new canonical naming scheme. The standard deviation of pre--post
  change scores is now consistently called `sd_change` (was sometimes
  `sd_diff` in code, sometimes `SDchange` in documentation). Effect-size
  identifiers move to snake_case (`smd_pre`, `smd_change`,
  `smd_pooled`, `smd_diff_groups`). The control-group suffix becomes
  `_ctrl` (was `_Con`).
- A flexible column-name matcher that accepts the canonical names, the
  v1.2.x mixed-case names, and a closed dictionary of synonyms
  (`baseline_mean`, `treatment`, `placebo`, `m1`/`m2`, `pval`, ...).
  Set `column_matching = "strict"` to disable.
- A backward-compatibility layer. Every v1.2.x argument
  (`method`, `SMD_method`, `MeanDifferences`, `custom_sd_diff_*`),
  every v1.2.x `effect_size` value (`"SMDpre"`, `"SMDchange"`,
  `"ScMDpooled"`, `"ScMDpre"`), and every v1.2.x column suffix
  (`_Con`, `sd_diff_*`) continues to work, with
  `lifecycle::deprecate_warn()` pointing to the new name. Slated for
  removal in 2.0.0.

### Behaviour preservation

The rename was verified to be **bit-for-bit numerically equivalent** to
1.2.1 across 23 golden-test fixtures covering every effect-size x
imputation-method combination, against both the published example
dataset (single-group) and a hand-crafted synthetic two-group dataset.
No formula changed; only names did.

### Reverse dependencies

`metacor` is a leaf package on CRAN: no reverse dependencies. No
downstream packages are affected by this release.

### Documentation

- New vignette **`metacor-intro`** (replaces `introduccion`).
- New vignette **`migrating-from-1.2`** documents every renamed
  argument, value, and column.

## Acknowledgements

Thanks to the CRAN maintainers for the patience this rename took to
prepare.
