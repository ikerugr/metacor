# Tests for the internal column matcher (R/match_columns.R + R/synonyms.R).
#
# These tests exercise match_columns() in isolation, before it is wired
# into metacor_dual(). They are the contract that the matcher must satisfy.

# Helper: build a one-row data frame with the given column names. The
# column values don't matter for the matcher; using sequential integers
# keeps the data frames easy to print on test failure.
make_df <- function(...) {
  cols <- c(...)
  d <- as.data.frame(matrix(seq_along(cols), nrow = 1L))
  names(d) <- cols
  d
}

# -----------------------------------------------------------------------
# Layer 1 — normalisation
# -----------------------------------------------------------------------

test_that("normalize_name strips separators and lowercases", {
  expect_equal(normalize_name("MeanPre_Int"),       "meanpreint")
  expect_equal(normalize_name("mean.pre.int"),      "meanpreint")
  expect_equal(normalize_name("mean-pre-int"),      "meanpreint")
  expect_equal(normalize_name("mean pre int"),      "meanpreint")
  expect_equal(normalize_name("MEANPREINT"),        "meanpreint")
  expect_equal(normalize_name("Mean_Pre_Int"),      "meanpreint")
})

test_that("ends_with handles equal-length and shorter suffixes", {
  expect_true(ends_with("meanpreint", "int"))
  expect_true(ends_with("meanpreint", "meanpreint"))
  expect_false(ends_with("int", "meanpreint"))
  expect_false(ends_with("meanpreintervention", "int"))  # ends in "tion"
})

# -----------------------------------------------------------------------
# Exact canonical names — silent passthrough
# -----------------------------------------------------------------------

test_that("exact canonical column names are not renamed and produce no messages", {
  df <- make_df("study_name", "mean_pre_int", "mean_post_int", "sd_pre_int",
                "sd_post_int", "sd_change_int", "n_int", "p_value_int",
                "upper_ci_int", "lower_ci_int")
  res <- match_columns(df, single_group = TRUE)
  expect_equal(names(res$df), names(df))
  expect_equal(nrow(res$mapping[res$mapping$kind == "exact", ]), 10L)
  expect_length(res$messages, 0L)
  expect_length(res$warnings, 0L)
})

# -----------------------------------------------------------------------
# Case / separator normalisation — silent rename
# -----------------------------------------------------------------------

test_that("case-only differences rename silently (no info message, no warning)", {
  df <- make_df("MeanPre_Int", "Mean_Post_Int", "n_INT", "Sd.Pre.Int")
  res <- match_columns(df, single_group = TRUE)
  expect_setequal(names(res$df),
                  c("mean_pre_int", "mean_post_int", "n_int", "sd_pre_int"))
  expect_true(all(res$mapping$kind == "case_only"))
  # case_only matches are silent
  expect_length(res$messages, 0L)
  expect_length(res$warnings, 0L)
})

# -----------------------------------------------------------------------
# Synonym matches — info messages, no warnings
# -----------------------------------------------------------------------

test_that("synonym matches are reported via messages, not warnings", {
  df <- make_df("baseline_mean_treatment",
                "follow_up_mean_treatment",
                "sample_size_treatment",
                "pval_treatment")
  res <- match_columns(df, single_group = FALSE)
  expect_setequal(names(res$df),
                  c("mean_pre_int", "mean_post_int", "n_int", "p_value_int"))
  expect_true(all(res$mapping$kind == "synonym"))
  expect_length(res$warnings, 0L)
  # The header line + one bullet per match + the trailing hint = >=4 lines
  expect_gt(length(res$messages), 4L)
  expect_match(res$messages[1], "Matched 4 column")
})

test_that("study_name passthrough synonyms are recognised", {
  df <- make_df("Study", "n_int")
  res <- match_columns(df, single_group = TRUE)
  expect_equal(names(res$df), c("study_name", "n_int"))
  expect_equal(res$mapping$kind[res$mapping$canonical == "study_name"],
               "synonym")
})

# -----------------------------------------------------------------------
# Legacy v1.2.x matches — deprecation warning
# -----------------------------------------------------------------------

test_that("legacy sd_diff stem triggers a deprecation warning", {
  df <- make_df("study_name", "sd_diff_int", "n_int")
  res <- match_columns(df, single_group = TRUE)
  expect_equal(names(res$df), c("study_name", "sd_change_int", "n_int"))
  expect_equal(res$mapping$kind[res$mapping$user_name == "sd_diff_int"],
               "legacy_v12")
  expect_length(res$warnings, 1L)
  expect_match(res$warnings, "v1\\.2\\.x")
  expect_match(res$warnings, "sd_change_int")
})

test_that("legacy _Con suffix triggers a deprecation warning and renames to _ctrl", {
  df <- make_df("meanPre_Int", "meanPre_Con", "n_Int", "n_Con")
  res <- match_columns(df, single_group = FALSE)
  expect_setequal(names(res$df),
                  c("mean_pre_int", "mean_pre_ctrl", "n_int", "n_ctrl"))
  # _Int collapses to _int via normalisation alone -> case_only, silent.
  # _Con is genuinely a v1.2.x deprecated name -> legacy_v12, warning.
  legacy_rows <- res$mapping[res$mapping$kind == "legacy_v12", ]
  expect_setequal(legacy_rows$user_name, c("meanPre_Con", "n_Con"))
  expect_length(res$warnings, 2L)
  expect_true(all(grepl("_ctrl", res$warnings)))
})

# -----------------------------------------------------------------------
# Single-group mode
# -----------------------------------------------------------------------

test_that("single_group=TRUE accepts unsuffixed columns as _int", {
  df <- make_df("study_name", "meanPre", "meanPost", "n", "sd_pre", "sd_post")
  res <- match_columns(df, single_group = TRUE)
  expect_setequal(names(res$df),
                  c("study_name", "mean_pre_int", "mean_post_int",
                    "n_int", "sd_pre_int", "sd_post_int"))
  expect_length(res$warnings, 0L)
})

test_that("single_group=TRUE drops columns resolving to _ctrl with a warning", {
  df <- make_df("study_name", "mean_pre_int", "mean_pre_ctrl", "n_int", "n_ctrl")
  res <- match_columns(df, single_group = TRUE)
  expect_setequal(names(res$df),
                  c("study_name", "mean_pre_int", "n_int"))
  expect_length(res$warnings, 2L)
  expect_true(all(grepl("single_group = TRUE", res$warnings)))
})

# -----------------------------------------------------------------------
# Strict mode
# -----------------------------------------------------------------------

test_that("strict mode does not rename anything", {
  df <- make_df("MeanPre_Int", "n_Treatment", "study")
  res <- match_columns(df, mode = "strict", single_group = TRUE)
  expect_equal(names(res$df), c("MeanPre_Int", "n_Treatment", "study"))
  expect_equal(nrow(res$mapping), 0L)
  expect_length(res$messages, 0L)
  expect_length(res$warnings, 0L)
})

# -----------------------------------------------------------------------
# Ambiguity: hard errors
# -----------------------------------------------------------------------

test_that("two user columns mapping to the same canonical raise an error", {
  df <- make_df("mean_pre_int", "MeanPre_Int")  # both -> mean_pre_int
  expect_error(
    match_columns(df, single_group = TRUE),
    "ambiguous"
  )
})

test_that("legacy and canonical for the same concept collide and raise an error", {
  df <- make_df("sd_diff_int", "sd_change_int")  # both -> sd_change_int
  expect_error(
    match_columns(df, single_group = TRUE),
    "ambiguous"
  )
})

# -----------------------------------------------------------------------
# Pass-through: non-metacor columns are left untouched
# -----------------------------------------------------------------------

test_that("non-metacor columns pass through unchanged and are not in mapping", {
  df <- make_df("study_name", "year", "country", "notes",
                "mean_pre_int", "n_int")
  res <- match_columns(df, single_group = TRUE)
  expect_equal(names(res$df), names(df))
  expect_setequal(res$mapping$user_name,
                  c("study_name", "mean_pre_int", "n_int"))
  expect_false("year" %in% res$mapping$user_name)
})

test_that("ambiguous-looking short tokens (e.g. 'pi', 'nt') pass through", {
  df <- make_df("pi", "nt", "ci")
  res <- match_columns(df, single_group = TRUE)
  expect_equal(names(res$df), c("pi", "nt", "ci"))
  expect_equal(nrow(res$mapping), 0L)
})

# -----------------------------------------------------------------------
# Edge cases
# -----------------------------------------------------------------------

test_that("empty data frame returns empty result without erroring", {
  df <- data.frame()
  res <- match_columns(df, single_group = TRUE)
  expect_equal(names(res$df), character(0L))
  expect_equal(nrow(res$mapping), 0L)
})

test_that("non-data-frame input is rejected", {
  expect_error(match_columns(list(a = 1L), single_group = TRUE),
               "data frame")
})

test_that("intervention synonyms (treatment/trt/exp/intervention) all resolve to _int", {
  for (suf in c("_intervention", "_treatment", "_trt", "_exp")) {
    df <- make_df(paste0("meanPre", suf))
    res <- match_columns(df, single_group = FALSE)
    expect_equal(names(res$df), "mean_pre_int",
                 info = paste("suffix:", suf))
  }
})

test_that("control synonyms (control/placebo/pbo/sham/passive) all resolve to _ctrl", {
  for (suf in c("_control", "_placebo", "_pbo", "_sham", "_passive")) {
    df <- make_df(paste0("meanPre", suf))
    res <- match_columns(df, single_group = FALSE)
    expect_equal(names(res$df), "mean_pre_ctrl",
                 info = paste("suffix:", suf))
  }
})

test_that("mean1/mean2 resolve to mean_pre/mean_post stems", {
  df <- make_df("mean1_int", "mean2_int", "mean1_ctrl", "mean2_ctrl")
  res <- match_columns(df, single_group = FALSE)
  expect_setequal(names(res$df),
                  c("mean_pre_int", "mean_post_int",
                    "mean_pre_ctrl", "mean_post_ctrl"))
  expect_true(all(res$mapping$kind == "synonym"))
})

test_that("longest-suffix-first rule disambiguates overlapping suffixes", {
  # "intervention" ends with both "intervention" (length 12) and "n" but
  # "n" is not a recognised suffix synonym, so this is moot. Check the
  # interesting case: suffixes "int" and "intervention" both syntactically
  # apply when the column ends in "intervention". The longer must win,
  # otherwise the stem becomes "meanpreintervent" which matches no canonical
  # stem and the column would silently pass through.
  df <- make_df("mean_pre_intervention")
  res <- match_columns(df, single_group = FALSE)
  expect_equal(names(res$df), "mean_pre_int")
})

# -----------------------------------------------------------------------
# Realistic scenarios — mixed inputs
# -----------------------------------------------------------------------

test_that("a realistic v1.2.x-style data frame is fully migrated with one warning per legacy name", {
  df <- make_df("study_name",
                # intervention group, v1.2.x form
                "meanPre_Int", "meanPost_Int", "sd_pre_Int", "sd_post_Int",
                "sd_diff_Int", "n_Int", "p_value_Int", "upperCI_Int", "lowerCI_Int",
                # control group, v1.2.x form
                "meanPre_Con", "meanPost_Con", "sd_pre_Con", "sd_post_Con",
                "sd_diff_Con", "n_Con", "p_value_Con", "upperCI_Con", "lowerCI_Con")
  res <- match_columns(df, single_group = FALSE)

  expect_setequal(
    names(res$df),
    c("study_name",
      "mean_pre_int", "mean_post_int", "sd_pre_int", "sd_post_int",
      "sd_change_int", "n_int", "p_value_int", "upper_ci_int", "lower_ci_int",
      "mean_pre_ctrl", "mean_post_ctrl", "sd_pre_ctrl", "sd_post_ctrl",
      "sd_change_ctrl", "n_ctrl", "p_value_ctrl", "upper_ci_ctrl", "lower_ci_ctrl")
  )

  legacy <- res$mapping[res$mapping$kind == "legacy_v12", ]
  # legacy: sd_diff_Int (stem), sd_diff_Con (stem AND suffix), and every _Con (suffix)
  # Every column with a `Con` suffix is legacy; every column with `sd_diff` stem
  # is legacy. Total expected legacy rows = 9 (Con cols) + 1 (sd_diff_Int) = 10.
  # Note sd_diff_Con is counted once even though both parts are legacy.
  expect_equal(nrow(legacy), 10L)
  expect_length(res$warnings, 10L)
})
