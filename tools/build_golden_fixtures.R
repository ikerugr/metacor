# tools/build_golden_fixtures.R
#
# Builds golden test fixtures by capturing the output of metacor_dual()
# and check_metacor_consistency() under a fixed set of argument combinations.
#
# Run from the package root (Rscript tools/build_golden_fixtures.R) whenever
# you intentionally want to bless a new "ground truth" — e.g. before starting
# a refactor, or after a deliberate behavioural change.
#
# The script:
#   1. Reads the single-group example dataset from `paper/metacor analysis Paper/`
#      and writes it as a portable CSV in `tests/testthat/_data/`.
#   2. Builds a deterministic synthetic two-group dataset and writes it likewise.
#   3. Runs metacor_dual() across a curated grid of arguments and serialises
#      each result as `tests/testthat/_golden/<scenario>.rds` together with
#      captured warnings and messages.
#
# Determinism note: metacor_dual() has no internal randomness, so the captured
# fixtures are bit-for-bit reproducible. We still set a seed defensively.
#
# IMPORTANT: this script loads the package source via devtools::load_all(),
# not the installed package. The fixtures therefore reflect the *current*
# state of R/.
#
# ---------------------------------------------------------------------------

set.seed(20251128L)

suppressPackageStartupMessages({
  library(devtools)
  library(readxl)
})

PKG_ROOT <- normalizePath(".")
DATA_DIR <- file.path(PKG_ROOT, "tests", "testthat", "_data")
GOLDEN_DIR <- file.path(PKG_ROOT, "tests", "testthat", "_golden")
EXAMPLE_XLSX <- file.path(
  PKG_ROOT, "..", "paper", "metacor analysis Paper", "Example Metacor Paper.xlsx"
)
stopifnot(dir.exists(DATA_DIR), dir.exists(GOLDEN_DIR), file.exists(EXAMPLE_XLSX))

devtools::load_all(PKG_ROOT, quiet = TRUE)
cat("Loaded metacor source. Version:", as.character(packageVersion("metacor")), "\n")

# ---------------------------------------------------------------------------
# 1. Single-group fixture: the canonical example dataset used in the paper
# ---------------------------------------------------------------------------
example_sg <- as.data.frame(read_excel(EXAMPLE_XLSX, sheet = "Sheet1 (2)"))
# the lowerCI/upperCI columns load as logical NA because the column is empty;
# coerce to numeric so the in-memory schema is stable.
example_sg$lowerCI_Int <- as.numeric(example_sg$lowerCI_Int)
example_sg$upperCI_Int <- as.numeric(example_sg$upperCI_Int)

# Use .rds rather than .csv to preserve column types exactly (especially
# all-NA numeric columns, which CSV would round-trip as logical).
saveRDS(example_sg, file.path(DATA_DIR, "example_single_group.rds"), version = 2L)
cat("Wrote example_single_group.rds:", nrow(example_sg), "rows.\n")

# ---------------------------------------------------------------------------
# 2. Two-group fixture (synthetic). Hand-crafted to exercise:
#    - rows with p_value only,
#    - rows with CI only,
#    - rows with both,
#    - rows with neither (forcing imputation),
#    - rows where r ends up out of range,
#    - one row where the control group has no info to impute.
# ---------------------------------------------------------------------------
example_tg <- data.frame(
  study_name   = paste0("Synth", sprintf("%02d", 1:10)),
  # Intervention group
  p_value_Int  = c(0.001, NA,    NA,    0.02,  NA,    NA,    0.05,  0.0001, NA,    NA),
  n_Int        = c(   12,    15,    10,    20,    14,    11,    18,     16,    13,    12),
  meanPre_Int  = c(  100,   55.0, 9.20,  120,    8.5,  60.0,   45,     200,   30.0, 15.0),
  meanPost_Int = c(  108,   60.5, 10.10, 132,    9.6,  64.0,   47,     220,   33.5, 17.0),
  sd_pre_Int   = c(   12,    6.0, 0.80,  18,     1.0,   8.0,   6.0,     25,    4.0,  2.0),
  sd_post_Int  = c(   13,    6.5, 0.85,  19,     1.1,   8.5,   6.5,     26,    4.2,  2.2),
  upperCI_Int  = c(   NA,   8.5,    NA,   NA,   1.6,    NA,   3.5,      NA,   5.5,   3.5),
  lowerCI_Int  = c(   NA,   2.5,    NA,   NA,   0.6,    NA,   0.5,      NA,   1.5,   0.5),
  # Control group
  p_value_Con  = c(0.40,    NA,    NA,    NA,    NA,   0.50,    NA,    0.30, NA,     NA),
  n_Con        = c(  12,    15,    10,    20,    14,    11,    18,      16,   13,    12),
  meanPre_Con  = c( 102,    54.0, 9.10,  118,    8.4,  61.0,   46,     198,  30.5,  15.5),
  meanPost_Con = c( 103,    54.8, 9.30,  119,    8.5,  61.2,   46.5,   202,  31.0,  15.7),
  sd_pre_Con   = c(  11,    6.2, 0.75,   17,     0.9,   7.5,   5.5,     24,   4.1,   2.1),
  sd_post_Con  = c(  12,    6.4, 0.80,   17,     0.95,  7.8,   5.7,     25,   4.2,   2.2),
  upperCI_Con  = c(  NA,    1.5,    NA,   NA,    0.4,    NA,    1.5,    NA,   1.0,    NA),
  lowerCI_Con  = c(  NA,    0.1,    NA,   NA,   -0.2,    NA,   -0.5,    NA,   0.0,    NA),
  stringsAsFactors = FALSE
)
saveRDS(example_tg, file.path(DATA_DIR, "example_two_group.rds"), version = 2L)
cat("Wrote example_two_group.rds:", nrow(example_tg), "rows.\n")

# ---------------------------------------------------------------------------
# 3. README so future maintainers know where these came from
# ---------------------------------------------------------------------------
writeLines(c(
  "# Test data fixtures",
  "",
  "These CSVs are inputs for the golden tests in `tests/testthat/`.",
  "",
  "## example_single_group.rds",
  "",
  "Extracted from `paper/metacor analysis Paper/Example Metacor Paper.xlsx`,",
  "sheet `Sheet1 (2)`. This is the dataset used in the `metacor` manuscript.",
  "Single-group pre-post design (intervention only). 11 studies.",
  "",
  "Stored as .rds (not .csv) so all-NA numeric columns keep their `numeric`",
  "type instead of round-tripping through `logical`.",
  "",
  "Regenerate with `Rscript tools/build_golden_fixtures.R`.",
  "",
  "## example_two_group.rds",
  "",
  "**Synthetic** dataset hand-crafted to exercise the control-group branch of",
  "`metacor_dual()`. Not real data; do not cite. Designed to cover rows with",
  "p-values only, CIs only, both, and neither (to force imputation).",
  "",
  "Regenerate with `Rscript tools/build_golden_fixtures.R`."
), file.path(DATA_DIR, "README.md"))

# ---------------------------------------------------------------------------
# 4. Helper: capture a metacor_dual() call together with its messages/warnings
# ---------------------------------------------------------------------------
capture_run <- function(label, ...) {
  msgs  <- character()
  warns <- character()
  res <- withCallingHandlers(
    metacor_dual(...),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(label = label, result = res, messages = msgs, warnings = warns)
}

save_golden <- function(label, payload) {
  path <- file.path(GOLDEN_DIR, paste0(label, ".rds"))
  saveRDS(payload, path, version = 2L)
  cat(sprintf("  golden: %-50s [%d rows, %d cols, %d msg, %d warn]\n",
              basename(path),
              nrow(payload$result), ncol(payload$result),
              length(payload$messages), length(payload$warnings)))
}

# ---------------------------------------------------------------------------
# 5. Capture single-group scenarios
#    Grid: effect_size {SMDpre, SMDchange} x impute_method {none, direct, mean, cv}
#    Plus: custom imputation, MeanDifferences=TRUE, digits variants
# ---------------------------------------------------------------------------
cat("\n== Single-group golden cases ==\n")
sg <- example_sg

for (es in c("SMDpre", "SMDchange")) {
  for (im in c("none", "direct", "mean", "cv")) {
    label <- sprintf("sg_%s_%s", tolower(es), im)
    payload <- capture_run(
      label,
      df = sg, digits = 3, add_to_df = TRUE, method = "both",
      apply_hedges = TRUE, SMD_method = es, MeanDifferences = FALSE,
      impute_method = im, verbose = TRUE, report_imputations = FALSE,
      single_group = TRUE
    )
    save_golden(label, payload)
  }
}

# Custom sd_diff_int + cv (mirrors r2 in the paper script)
payload <- capture_run(
  "sg_smdpre_cv_custom",
  df = sg, digits = 3, add_to_df = TRUE, method = "both",
  apply_hedges = TRUE, SMD_method = "SMDpre", MeanDifferences = FALSE,
  impute_method = "cv", verbose = TRUE, report_imputations = FALSE,
  custom_sd_diff_int = list(list(row = 7, value = 0.5)),
  single_group = TRUE
)
save_golden("sg_smdpre_cv_custom", payload)

# MeanDifferences = TRUE (mirrors r4 in the paper script)
payload <- capture_run(
  "sg_smdchange_cv_meandiffs",
  df = sg, digits = 3, add_to_df = TRUE, method = "both",
  apply_hedges = TRUE, SMD_method = "SMDchange", MeanDifferences = TRUE,
  impute_method = "cv", verbose = TRUE, report_imputations = FALSE,
  single_group = TRUE
)
save_golden("sg_smdchange_cv_meandiffs", payload)

# digits = NULL (no rounding) — sanity check that round-trip works
payload <- capture_run(
  "sg_smdpre_cv_no_round",
  df = sg, digits = NULL, add_to_df = TRUE, method = "both",
  apply_hedges = TRUE, SMD_method = "SMDpre", MeanDifferences = FALSE,
  impute_method = "cv", verbose = FALSE, report_imputations = FALSE,
  single_group = TRUE
)
save_golden("sg_smdpre_cv_no_round", payload)

# apply_hedges = FALSE
payload <- capture_run(
  "sg_smdpre_cv_no_hedges",
  df = sg, digits = 3, add_to_df = TRUE, method = "both",
  apply_hedges = FALSE, SMD_method = "SMDpre", MeanDifferences = FALSE,
  impute_method = "cv", verbose = FALSE, report_imputations = FALSE,
  single_group = TRUE
)
save_golden("sg_smdpre_cv_no_hedges", payload)

# ---------------------------------------------------------------------------
# 6. Capture two-group scenarios on the synthetic dataset
# ---------------------------------------------------------------------------
cat("\n== Two-group golden cases ==\n")
tg <- example_tg

for (es in c("SMDpre", "SMDchange", "ScMDpooled", "ScMDpre")) {
  for (im in c("none", "cv")) {
    label <- sprintf("tg_%s_%s", tolower(es), im)
    payload <- capture_run(
      label,
      df = tg, digits = 4, add_to_df = TRUE, method = "both",
      apply_hedges = TRUE, SMD_method = es, MeanDifferences = TRUE,
      impute_method = im, verbose = FALSE, report_imputations = FALSE,
      single_group = FALSE
    )
    save_golden(label, payload)
  }
}

# Custom imputation on both groups
payload <- capture_run(
  "tg_smdchange_custom_both",
  df = tg, digits = 4, add_to_df = TRUE, method = "both",
  apply_hedges = TRUE, SMD_method = "SMDchange", MeanDifferences = FALSE,
  impute_method = "none", verbose = FALSE, report_imputations = FALSE,
  custom_sd_diff_int = list(list(row = 3, value = 0.6),
                            list(row = 6, value = 5.0)),
  custom_sd_diff_con = list(list(row = 3, value = 0.5),
                            list(row = 6, value = 4.5)),
  single_group = FALSE
)
save_golden("tg_smdchange_custom_both", payload)

# ---------------------------------------------------------------------------
# 7. Capture check_metacor_consistency() outputs (single-group + two-group)
# ---------------------------------------------------------------------------
cat("\n== Consistency check golden cases ==\n")

base_sg <- metacor_dual(
  sg, digits = 4, add_to_df = TRUE, method = "both",
  apply_hedges = TRUE, SMD_method = "SMDpre", MeanDifferences = TRUE,
  impute_method = "cv", verbose = FALSE, report_imputations = FALSE,
  single_group = TRUE
)
chk_sg <- check_metacor_consistency(base_sg, interpret = TRUE)
saveRDS(list(label = "consistency_sg", result = chk_sg),
        file.path(GOLDEN_DIR, "consistency_sg.rds"), version = 2L)
cat("  golden: consistency_sg.rds [", nrow(chk_sg), "x", ncol(chk_sg), "]\n")

base_tg <- metacor_dual(
  tg, digits = 4, add_to_df = TRUE, method = "both",
  apply_hedges = TRUE, SMD_method = "SMDpre", MeanDifferences = TRUE,
  impute_method = "cv", verbose = FALSE, report_imputations = FALSE,
  single_group = FALSE
)
chk_tg <- check_metacor_consistency(base_tg, interpret = TRUE)
saveRDS(list(label = "consistency_tg", result = chk_tg),
        file.path(GOLDEN_DIR, "consistency_tg.rds"), version = 2L)
cat("  golden: consistency_tg.rds [", nrow(chk_tg), "x", ncol(chk_tg), "]\n")

cat("\nDone. ", length(list.files(GOLDEN_DIR, pattern = "\\.rds$")),
    "golden fixtures written to", GOLDEN_DIR, "\n")
