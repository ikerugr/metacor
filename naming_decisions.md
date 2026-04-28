# Naming Decisions — metacor v1.3.0

This is the source of truth for naming, deprecation, and column-matching
behaviour during the v1.2.1 → v1.3.0 → v2.0.0 transition. Every code change,
test, NEWS entry, vignette and paper revision must be consistent with this
document. If a real-world need arises that contradicts a decision here, update
this file *first*, then propagate.

---

## 1. Context and goals

The CRAN package `metacor` (v1.2.1) accumulated three naming inconsistencies
over time:

1. **`SDdiff` vs `SDchange`** — the same concept (the SD of pre–post difference
   scores) is called both ways across code, docs, and the manuscript. The
   canonical term in the methodological literature (Cochrane Handbook §6.5.2.8;
   Borenstein et al., 2009) is **SD of change scores**.
2. **Mixed casing styles** — `MeanDifferences` (CamelCase) coexists with
   `apply_hedges` and `impute_method` (snake_case).
3. **Internal mismatch** — the effect size `SMDchange` is computed using a
   denominator named `sd_diff_int`. The two should share one root word.

The v1.3.0 release introduces a single canonical vocabulary and a backwards
compatibility layer. The v2.0.0 release (≈6 months after 1.3.0 lands on CRAN)
removes the compatibility layer.

---

## 2. Canonical vocabulary

The single approved root word for each concept:

| Concept | Canonical term (text) | Canonical term (code) |
|---|---|---|
| Standard deviation of pre–post change scores | **SD of change** / `SD_change` | `sd_change` |
| Pearson correlation pre–post | `r` | `r` |
| Standardised mean difference (pre as denominator) | `SMD_pre` | `smd_pre` |
| Standardised mean difference (SD of change as denominator) | `SMD_change` | `smd_change` |
| Standardised change mean difference, pooled SD | `SMD_pooled` | `smd_pooled` |
| Standardised difference of changes between groups | `SMD_diff_groups` | `smd_diff_groups` |
| Intervention/treatment group suffix | `int` | `_int` |
| Control group suffix | `ctrl` | `_ctrl` |

**Forbidden going forward:** `SDdiff`, `sd_diff`, `SDchange` (mixed-case),
`ScMDpooled`, `ScMDpre`, `_Int`, `_Con`, `MeanDifferences`.

---

## 3. Function arguments — old → new

### `metacor_dual()`

| v1.2.1 | v1.3.0 | Notes |
|---|---|---|
| `method` | `derive_from` | `method` was ambiguous (3 args contained "method"). |
| `SMD_method` | `effect_size` | Describes what the user picks. |
| `MeanDifferences` | `mean_differences` | snake_case throughout. |
| `custom_sd_diff_int` | `custom_sd_change_int` | "diff" → "change". |
| `custom_sd_diff_con` | `custom_sd_change_ctrl` | "diff" → "change", "con" → "ctrl". |
| `apply_hedges`, `impute_method`, `single_group`, `report_imputations`, `verbose`, `digits`, `add_to_df` | unchanged | |

### Argument values

| Argument | v1.2.1 values | v1.3.0 values |
|---|---|---|
| `derive_from` (was `method`) | `"p_value"`, `"ci"`, `"both"` | unchanged |
| `effect_size` (was `SMD_method`) | `"SMDpre"`, `"SMDchange"`, `"ScMDpooled"`, `"ScMDpre"` | `"smd_pre"`, `"smd_change"`, `"smd_pooled"`, `"smd_diff_groups"` |
| `impute_method` | `"none"`, `"direct"`, `"mean"`, `"cv"` | unchanged |

---

## 4. Input columns expected from the user

| v1.2.1 | v1.3.0 |
|---|---|
| `meanPre_Int`, `meanPost_Int` | `meanPre_int`, `meanPost_int` |
| `sd_pre_Int`, `sd_post_Int` | `sd_pre_int`, `sd_post_int` |
| `n_Int`, `p_value_Int` | `n_int`, `p_value_int` |
| `upperCI_Int`, `lowerCI_Int` | `upperCI_int`, `lowerCI_int` |
| `meanPre_Con` … `lowerCI_Con` | `meanPre_ctrl` … `lowerCI_ctrl` |

The user is **not required** to rename their columns manually — the column
matcher (§6) handles legacy `_Int`/`_Con` suffixes automatically and emits a
deprecation warning.

---

## 5. Output columns added by the package

| v1.2.1 | v1.3.0 |
|---|---|
| `sd_diff_int`, `sd_diff_con` | `sd_change_int`, `sd_change_ctrl` |
| `r_int`, `r_con` | `r_int`, `r_ctrl` |
| `pct_change_int`, `pct_change_con` | `pct_change_int`, `pct_change_ctrl` |
| `SMDpre_int`, `varSMDpre_int`, `seSMDpre_int` | `smd_pre_int`, `var_smd_pre_int`, `se_smd_pre_int` |
| `SMDchange_int`, `varSMDchange_int`, `seSMDchange_int` | `smd_change_int`, `var_smd_change_int`, `se_smd_change_int` |
| `ScMDpooled`, `varScMDpooled`, `seScMDpooled` | `smd_pooled`, `var_smd_pooled`, `se_smd_pooled` |
| `ScMDpre`, `varScMDpre`, `seScMDpre` | `smd_diff_groups`, `var_smd_diff_groups`, `se_smd_diff_groups` |
| `meanDiff_int`, `varMeanDiff_int`, `seMeanDiff_int` | `mean_diff_int`, `var_mean_diff_int`, `se_mean_diff_int` |
| `flag_sd_diff_out_of_range_int/con` | `flag_sd_change_out_of_range_int/ctrl` |

The same renaming applies to the `*_con` → `*_ctrl` pairs.

---

## 6. Flexible column matching

`metacor_dual()` gains a new argument:

```r
column_matching = c("flexible", "strict")
```

Default: `"flexible"`. The matcher resolves user column names to canonical
names in two layers, logs every resolution, and errors on ambiguity.

### 6.1 Layer 1 — normalisation (always applied)

Both the user's column name and each canonical name are normalised before
comparison:

- lowercase
- remove separators: `_`, `-`, `.`, whitespace

Examples (all match the same canonical column):

```
meanPre_int   meanPreInt   mean_pre_int   Mean.Pre.Int   "mean pre int"
```

This layer cannot produce false positives — it only collapses formatting
differences.

### 6.2 Layer 2 — explicit synonym dictionary (always applied)

Closed, documented list. **Only entries listed here match.** No Levenshtein,
no general fuzzy matching.

#### Stem synonyms

| Canonical stem | Accepted synonyms |
|---|---|
| `meanPre` | `mean_pre`, `pre_mean`, `m_pre`, `meanbaseline`, `baseline_mean`, `pre`, `m1` |
| `meanPost` | `mean_post`, `post_mean`, `m_post`, `meanfollowup`, `followup_mean`, `post`, `m2` |
| `sd_pre` | `sdpre`, `sd_baseline`, `sd1`, `s_pre` |
| `sd_post` | `sdpost`, `sd_followup`, `sd2`, `s_post` |
| `sd_change` | `sd_diff`, `sd_difference`, `sdchange`, `sd_d`, `sdc`, `sd_of_change` |
| `n` | `n_total`, `sample_size`, `samplesize`, `nsubjects` |
| `p_value` | `pvalue`, `pval`, `sig` |
| `upperCI` | `upper_ci`, `ciupper`, `ci_upper`, `ub`, `upperbound`, `ci_high` |
| `lowerCI` | `lower_ci`, `cilower`, `ci_lower`, `lb`, `lowerbound`, `ci_low` |

**Explicitly rejected as too short / ambiguous:** `p` (alone), `c`, `e`, `i`,
`t` — they can collide with t-statistics, CI columns, generic group letters.

#### Group suffix synonyms

| Canonical suffix | Accepted synonyms |
|---|---|
| `_int` | `_intervention`, `_treatment`, `_trt`, `_exp`, `_Int` |
| `_ctrl` | `_control`, `_con`, `_Con`, `_placebo`, `_pbo` |

The legacy `_Int` and `_Con` are listed here so existing user code Just Works
under `column_matching = "flexible"` — but the matcher emits a one-time
deprecation warning per call.

### 6.3 Single-group mode

When `single_group = TRUE`:

- columns without a group suffix (e.g. `meanPre`) are accepted as `meanPre_int`.
- columns with a `ctrl` suffix are ignored with an informational message.

### 6.4 Reporting and ambiguity

Every resolved match is reported once per call:

```
ℹ Matched columns to canonical names:
  • "Mean_Pre_Treatment"   → meanPre_int
  • "SD.diff.intervention" → sd_change_int
  • "Sample.Size.T"        → n_int
ℹ 3 columns matched flexibly. Use column_matching = "strict" to disable.
```

Ambiguity is a hard error, never a silent guess:

```
✖ Column matching is ambiguous:
  • "sd_pre" could match both sd_pre_int and sd_pre_ctrl.
  • "sample" could match both n_int and n_ctrl.
ℹ Either rename the columns explicitly, or pass column_matching = "strict".
```

Under `column_matching = "strict"`, only the canonical names listed in §4 are
accepted; anything else is a hard error.

---

## 7. Deprecation strategy

Implemented with `lifecycle::deprecate_warn()`. A single helper
`R/utils-deprecate.R::translate_legacy_args()` handles all migrations, called
once at the top of `metacor_dual()`.

| Layer | v1.3.0 behaviour | v2.0.0 behaviour |
|---|---|---|
| Old argument names (`method`, `SMD_method`, `MeanDifferences`, `custom_sd_diff_*`) | accepted, translated, deprecation warning | removed |
| Old argument values (`"SMDpre"`, `"SMDchange"`, `"ScMDpooled"`, `"ScMDpre"`) | accepted, translated, deprecation warning | removed |
| Legacy column suffixes (`_Int`, `_Con`) | matched via §6.2, deprecation warning | rejected unless `column_matching = "flexible"` |
| Old output column names | not produced; output uses §5 canonical names from v1.3.0 | unchanged |

Note: output column names change immediately in v1.3.0 (no aliasing). Anyone
relying on exact names (`df$sd_diff_int`) sees a hard NULL — this is by design.
A migration vignette spells out the rename.

---

## 8. Files affected by this document

- `R/metacor_dual.R` — argument renames, internal variable renames, output
  column renames.
- `R/check_metacor_consistency.R` — flag column renames, internal renames.
- `R/utils-deprecate.R` *(new)* — `translate_legacy_args()`, `translate_legacy_columns()`.
- `R/match_columns.R` *(new)* — flexible matcher.
- `R/synonyms.R` *(new)* — synonym dictionary as a single internal data
  structure (`SYNONYMS`).
- `man/*.Rd` — regenerated via roxygen.
- `vignettes/metacor-intro.Rmd` *(renamed from `introduccion.Rmd`)*.
- `vignettes/migrating-from-1.2.Rmd` *(new)*.
- `tests/testthat/` *(new)* — golden tests with the three validated datasets.
- `NEWS.md` *(new)* — retroactive 1.0–1.2 entries plus 1.3.0 changelog.
- `README.Rmd` / `README.md` — updated examples.
- `DESCRIPTION` — version bump 1.2.1 → 1.3.0; add `lifecycle` to Imports.

---

## 9. Out of scope (deferred)

These were considered and intentionally not addressed in v1.3.0:

- **Levenshtein / general fuzzy matching.** Risk of silently matching
  `sd_pre` to `sd_post` outweighs UX benefit. Closed synonym list only.
- **Splitting `metacor_dual()` into smaller exported functions** (e.g.
  `derive_sd_change()`, `impute_sd_change()`). Internal modularisation yes;
  exposing more entry points is a v2.x conversation, after we see if users
  actually want them.
- **Changing the imputation report from Word to Quarto/HTML.** Tracked
  separately; not blocking the rename release.
- **Renaming the package itself.** `metacor` is on CRAN with a citation; the
  name stays.
