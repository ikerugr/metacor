# tools/verify_numerical_equivalence.R
#
# One-shot validation script for the v1.2.1 -> v1.3.0 rename.
#
# Compares the OLD golden fixtures (stored at /tmp/_golden_v12_backup/) to
# the NEW golden fixtures (under tests/testthat/_golden/), after mapping
# old column names and old fixture-label names to their v1.3 canonical
# counterparts.
#
# If every fixture pair compares equal, the rename is provably
# behaviour-preserving: no number changed, only names.
#
# Usage:
#   Rscript tools/verify_numerical_equivalence.R
#
# After running successfully once, this script can be deleted.

suppressPackageStartupMessages({
  library(devtools)
})
devtools::load_all(".", quiet = TRUE)

OLD_DIR <- "/tmp/_golden_v12_backup"
NEW_DIR <- file.path("tests", "testthat", "_golden")
stopifnot(dir.exists(OLD_DIR), dir.exists(NEW_DIR))

# Filename mapping: old fixture label -> new fixture label.
old_to_new_label <- c(
  "sg_smdpre_none"             = "sg_smd_pre_none",
  "sg_smdpre_direct"           = "sg_smd_pre_direct",
  "sg_smdpre_mean"             = "sg_smd_pre_mean",
  "sg_smdpre_cv"               = "sg_smd_pre_cv",
  "sg_smdpre_cv_custom"        = "sg_smd_pre_cv_custom",
  "sg_smdpre_cv_no_round"      = "sg_smd_pre_cv_no_round",
  "sg_smdpre_cv_no_hedges"     = "sg_smd_pre_cv_no_hedges",
  "sg_smdchange_none"          = "sg_smd_change_none",
  "sg_smdchange_direct"        = "sg_smd_change_direct",
  "sg_smdchange_mean"          = "sg_smd_change_mean",
  "sg_smdchange_cv"            = "sg_smd_change_cv",
  "sg_smdchange_cv_meandiffs"  = "sg_smd_change_cv_meandiffs",
  "tg_smdpre_none"             = "tg_smd_pre_none",
  "tg_smdpre_cv"               = "tg_smd_pre_cv",
  "tg_smdchange_none"          = "tg_smd_change_none",
  "tg_smdchange_cv"            = "tg_smd_change_cv",
  "tg_scmdpooled_none"         = "tg_smd_pooled_none",
  "tg_scmdpooled_cv"           = "tg_smd_pooled_cv",
  "tg_scmdpre_none"            = "tg_smd_diff_groups_none",
  "tg_scmdpre_cv"              = "tg_smd_diff_groups_cv",
  "tg_smdchange_custom_both"   = "tg_smd_change_custom_both",
  "consistency_sg"             = "consistency_sg",
  "consistency_tg"             = "consistency_tg"
)

# Column-name mapping: every old column name -> v1.3 canonical name.
# Anything not listed is assumed to be unchanged.
col_rename_map <- c(
  # input columns (preserved by add_to_df = TRUE)
  "meanPre_Int"  = "mean_pre_int",
  "meanPost_Int" = "mean_post_int",
  "sd_pre_Int"   = "sd_pre_int",
  "sd_post_Int"  = "sd_post_int",
  "n_Int"        = "n_int",
  "p_value_Int"  = "p_value_int",
  "upperCI_Int"  = "upper_ci_int",
  "lowerCI_Int"  = "lower_ci_int",
  "meanPre_Con"  = "mean_pre_ctrl",
  "meanPost_Con" = "mean_post_ctrl",
  "sd_pre_Con"   = "sd_pre_ctrl",
  "sd_post_Con"  = "sd_post_ctrl",
  "n_Con"        = "n_ctrl",
  "p_value_Con"  = "p_value_ctrl",
  "upperCI_Con"  = "upper_ci_ctrl",
  "lowerCI_Con"  = "lower_ci_ctrl",
  # Output columns added by metacor_dual()
  "r_con"            = "r_ctrl",
  "sd_diff_int"      = "sd_change_int",
  "sd_diff_con"      = "sd_change_ctrl",
  "pct_change_con"   = "pct_change_ctrl",
  "SMDpre_int"       = "smd_pre_int",
  "SMDpre_con"       = "smd_pre_ctrl",
  "varSMDpre_int"    = "var_smd_pre_int",
  "varSMDpre_con"    = "var_smd_pre_ctrl",
  "seSMDpre_int"     = "se_smd_pre_int",
  "seSMDpre_con"     = "se_smd_pre_ctrl",
  "SMDchange_int"    = "smd_change_int",
  "SMDchange_con"    = "smd_change_ctrl",
  "varSMDchange_int" = "var_smd_change_int",
  "varSMDchange_con" = "var_smd_change_ctrl",
  "seSMDchange_int"  = "se_smd_change_int",
  "seSMDchange_con"  = "se_smd_change_ctrl",
  "ScMDpooled"       = "smd_pooled",
  "varScMDpooled"    = "var_smd_pooled",
  "seScMDpooled"     = "se_smd_pooled",
  "ScMDpre"          = "smd_diff_groups",
  "varScMDpre"       = "var_smd_diff_groups",
  "seScMDpre"        = "se_smd_diff_groups",
  "meanDiff_int"     = "mean_diff_int",
  "meanDiff_con"     = "mean_diff_ctrl",
  "varMeanDiff_int"  = "var_mean_diff_int",
  "varMeanDiff_con"  = "var_mean_diff_ctrl",
  "seMeanDiff_int"   = "se_mean_diff_int",
  "seMeanDiff_con"   = "se_mean_diff_ctrl",
  # Consistency-check columns
  "flag_sd_diff_out_of_range_int" = "flag_sd_change_out_of_range_int",
  "flag_sd_diff_out_of_range_con" = "flag_sd_change_out_of_range_ctrl",
  "flag_p_mismatch_con"           = "flag_p_mismatch_ctrl",
  "flag_CI_mismatch_con"          = "flag_CI_mismatch_ctrl",
  "flag_r_extreme_con"            = "flag_r_extreme_ctrl",
  "summary_con"                   = "summary_ctrl"
)

rename_old_columns <- function(df) {
  nm <- names(df)
  hits <- nm %in% names(col_rename_map)
  nm[hits] <- col_rename_map[nm[hits]]
  names(df) <- nm
  df
}

# Old narrative summaries used "pre-post difference"; new code uses
# "pre-post change". When comparing summary text, normalise the wording.
normalise_summary_text <- function(s) {
  s <- gsub("pre-post difference", "pre-post change", s, fixed = TRUE)
  s
}

# Compare two data frames for numerical equivalence after column rename.
compare_one <- function(label_old, label_new) {
  old <- readRDS(file.path(OLD_DIR, paste0(label_old, ".rds")))
  new <- readRDS(file.path(NEW_DIR, paste0(label_new, ".rds")))

  old_df <- rename_old_columns(old$result)
  new_df <- new$result

  # Normalise summary_int / summary_ctrl text differences (wording change
  # "difference" -> "change") that aren't real numerical changes.
  for (col in intersect(c("summary_int", "summary_ctrl"), names(old_df))) {
    old_df[[col]] <- normalise_summary_text(old_df[[col]])
  }

  # Reorder old_df columns to match new_df (we only care about value
  # equivalence, not column order).
  common_cols <- intersect(names(new_df), names(old_df))
  missing_in_new <- setdiff(names(old_df), names(new_df))
  missing_in_old <- setdiff(names(new_df), names(old_df))

  status <- list(label_old = label_old, label_new = label_new,
                 ok = TRUE, notes = character())

  if (length(missing_in_new) > 0L) {
    status$ok <- FALSE
    status$notes <- c(status$notes,
      sprintf("Columns in OLD missing from NEW: %s",
              paste(missing_in_new, collapse = ", ")))
  }
  if (length(missing_in_old) > 0L) {
    status$ok <- FALSE
    status$notes <- c(status$notes,
      sprintf("Columns in NEW missing from OLD: %s",
              paste(missing_in_old, collapse = ", ")))
  }

  for (col in common_cols) {
    a <- old_df[[col]]
    b <- new_df[[col]]
    if (length(a) != length(b)) {
      status$ok <- FALSE
      status$notes <- c(status$notes,
        sprintf("Length mismatch for '%s': %d vs %d", col, length(a), length(b)))
      next
    }
    eq <- if (is.numeric(a) && is.numeric(b)) {
      isTRUE(all.equal(a, b, tolerance = 1e-10))
    } else {
      identical(a, b)
    }
    if (!isTRUE(eq)) {
      status$ok <- FALSE
      diffs <- which(!mapply(function(x, y) identical(x, y) || (is.numeric(x) && is.numeric(y) && isTRUE(all.equal(x, y, tolerance = 1e-10))), a, b))
      status$notes <- c(status$notes,
        sprintf("Value mismatch in '%s' at row(s) %s",
                col, paste(diffs, collapse = ", ")))
    }
  }
  status
}

# ---------------------------------------------------------------------------
# Run all comparisons
# ---------------------------------------------------------------------------
cat("\n=== Numerical equivalence check (v1.2.x golden vs v1.3.0 golden) ===\n\n")

results <- list()
for (i in seq_along(old_to_new_label)) {
  lo <- names(old_to_new_label)[i]
  ln <- unname(old_to_new_label[i])
  results[[i]] <- compare_one(lo, ln)
}

n_ok   <- sum(vapply(results, function(r) r$ok,    logical(1)))
n_fail <- length(results) - n_ok
cat(sprintf("PASS: %d   FAIL: %d   TOTAL: %d\n\n", n_ok, n_fail, length(results)))

if (n_fail > 0L) {
  cat("FAILURES:\n")
  for (r in results) {
    if (!r$ok) {
      cat(sprintf("  %s -> %s\n", r$label_old, r$label_new))
      for (note in r$notes) cat("    *", note, "\n")
    }
  }
  quit(status = 1L)
} else {
  cat("All v1.2.x and v1.3.0 fixtures are numerically equivalent.\n")
  cat("The rename is provably behaviour-preserving.\n")
  invisible(NULL)
}
