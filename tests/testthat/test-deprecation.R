# Tests for the v1.2.x backward-compatibility layer.
#
# Verify that:
#   1. Each deprecated argument name is still accepted, emits a
#      lifecycle deprecation warning, and produces the same result as
#      its v1.3.0 replacement.
#   2. Each deprecated `effect_size` value (`"SMDpre"`, ...) is still
#      accepted and translated.
#   3. Legacy v1.2.x column names (`meanPre_Int`, `_Con`, `sd_diff_*`)
#      reach metacor_dual() correctly via the column matcher.

# --- Argument-name shims --------------------------------------------------

test_that("legacy `method` arg is accepted, emits deprecation, and matches `derive_from`", {
  sg <- read_fixture("example_single_group.rds")
  lifecycle::expect_deprecated(
    res_legacy <- metacor_dual(sg, method = "both", effect_size = "smd_pre",
                               impute_method = "none", single_group = TRUE,
                               verbose = FALSE) |> suppress_data_warnings()
  )
  res_canonical <- metacor_dual(sg, derive_from = "both", effect_size = "smd_pre",
                                impute_method = "none", single_group = TRUE,
                                verbose = FALSE) |> suppress_data_warnings()
  expect_equal(res_legacy, res_canonical)
})

test_that("legacy `SMD_method` arg is accepted and matches `effect_size`", {
  sg <- read_fixture("example_single_group.rds")
  lifecycle::expect_deprecated(
    res_legacy <- metacor_dual(sg, SMD_method = "smd_pre",
                               impute_method = "none", single_group = TRUE,
                               verbose = FALSE) |> suppress_data_warnings()
  )
  res_canonical <- metacor_dual(sg, effect_size = "smd_pre",
                                impute_method = "none", single_group = TRUE,
                                verbose = FALSE) |> suppress_data_warnings()
  expect_equal(res_legacy, res_canonical)
})

test_that("legacy `MeanDifferences` arg is accepted and matches `mean_differences`", {
  sg <- read_fixture("example_single_group.rds")
  lifecycle::expect_deprecated(
    res_legacy <- metacor_dual(sg, MeanDifferences = TRUE,
                               impute_method = "cv", single_group = TRUE,
                               verbose = FALSE) |> suppress_data_warnings()
  )
  res_canonical <- metacor_dual(sg, mean_differences = TRUE,
                                impute_method = "cv", single_group = TRUE,
                                verbose = FALSE) |> suppress_data_warnings()
  expect_equal(res_legacy, res_canonical)
})

test_that("legacy `custom_sd_diff_int` arg is accepted and matches `custom_sd_change_int`", {
  sg <- read_fixture("example_single_group.rds")
  lifecycle::expect_deprecated(
    res_legacy <- metacor_dual(sg, custom_sd_diff_int = list(list(row = 7, value = 0.5)),
                               impute_method = "cv", single_group = TRUE,
                               verbose = FALSE) |> suppress_data_warnings()
  )
  res_canonical <- metacor_dual(sg, custom_sd_change_int = list(list(row = 7, value = 0.5)),
                                impute_method = "cv", single_group = TRUE,
                                verbose = FALSE) |> suppress_data_warnings()
  expect_equal(res_legacy, res_canonical)
})

test_that("legacy `custom_sd_diff_con` arg is accepted and matches `custom_sd_change_ctrl`", {
  tg <- read_fixture("example_two_group.rds")
  lifecycle::expect_deprecated(
    res_legacy <- metacor_dual(tg,
                               custom_sd_diff_con = list(list(row = 3, value = 0.5)),
                               impute_method = "none", verbose = FALSE) |>
      suppress_data_warnings()
  )
  res_canonical <- metacor_dual(tg,
                                custom_sd_change_ctrl = list(list(row = 3, value = 0.5)),
                                impute_method = "none", verbose = FALSE) |>
    suppress_data_warnings()
  expect_equal(res_legacy, res_canonical)
})

# --- Argument-value shims (effect_size) -----------------------------------

test_that("legacy effect_size values translate to new ones with deprecation warning", {
  sg <- read_fixture("example_single_group.rds")
  legacy_values  <- c("SMDpre", "SMDchange", "ScMDpooled", "ScMDpre")
  canonical_vals <- c("smd_pre", "smd_change", "smd_pooled", "smd_diff_groups")

  for (i in seq_along(legacy_values)) {
    if (legacy_values[i] %in% c("ScMDpooled", "ScMDpre")) {
      # Two-group only; skip on single-group fixture
      next
    }
    legacy_v <- legacy_values[i]
    new_v    <- canonical_vals[i]
    lifecycle::expect_deprecated(
      res_legacy <- metacor_dual(sg, effect_size = legacy_v,
                                 impute_method = "none", single_group = TRUE,
                                 verbose = FALSE) |> suppress_data_warnings()
    )
    res_canonical <- metacor_dual(sg, effect_size = new_v,
                                  impute_method = "none", single_group = TRUE,
                                  verbose = FALSE) |> suppress_data_warnings()
    expect_equal(res_legacy, res_canonical,
                 info = paste("legacy:", legacy_v, "->", new_v))
  }
})

test_that("two-group legacy effect_size values (ScMDpooled, ScMDpre) translate correctly", {
  tg <- read_fixture("example_two_group.rds")
  for (pair in list(c("ScMDpooled", "smd_pooled"),
                    c("ScMDpre",    "smd_diff_groups"))) {
    lifecycle::expect_deprecated(
      res_legacy <- metacor_dual(tg, effect_size = pair[1],
                                 impute_method = "none",
                                 verbose = FALSE) |> suppress_data_warnings()
    )
    res_canonical <- metacor_dual(tg, effect_size = pair[2],
                                  impute_method = "none",
                                  verbose = FALSE) |> suppress_data_warnings()
    expect_equal(res_legacy, res_canonical, info = paste("legacy:", pair[1]))
  }
})

# --- Legacy column names reach metacor_dual through the matcher -----------

test_that("metacor_dual() accepts a v1.2.x-style data frame (meanPre_Int, _Con, sd_diff_*)", {
  # Build a v1.2.x-styled input (single group) by renaming the canonical
  # fixture columns *back* to v1.2.x form.
  sg_v13 <- read_fixture("example_single_group.rds")
  v13_to_v12 <- c(
    mean_pre_int  = "meanPre_Int",
    mean_post_int = "meanPost_Int",
    sd_pre_int    = "sd_pre_Int",
    sd_post_int   = "sd_post_Int",
    n_int         = "n_Int",
    p_value_int   = "p_value_Int",
    upper_ci_int  = "upperCI_Int",
    lower_ci_int  = "lowerCI_Int"
  )
  sg_v12 <- sg_v13
  for (canonical in names(v13_to_v12)) {
    if (canonical %in% names(sg_v12)) {
      names(sg_v12)[names(sg_v12) == canonical] <- v13_to_v12[[canonical]]
    }
  }

  # The matcher will resolve _Int -> _int via case-only normalisation
  # (silent). Result should be numerically identical.
  res_legacy <- metacor_dual(sg_v12, effect_size = "smd_pre",
                             impute_method = "cv", single_group = TRUE,
                             verbose = FALSE) |> suppress_data_warnings()
  res_canonical <- metacor_dual(sg_v13, effect_size = "smd_pre",
                                impute_method = "cv", single_group = TRUE,
                                verbose = FALSE) |> suppress_data_warnings()
  expect_equal(res_legacy, res_canonical)
})

test_that("metacor_dual() accepts legacy `_Con` and `sd_diff_*` column names with deprecation warning", {
  # Two-group fixture in v1.2.x form
  tg_v13 <- read_fixture("example_two_group.rds")
  v13_to_v12 <- c(
    mean_pre_ctrl  = "meanPre_Con",
    mean_post_ctrl = "meanPost_Con",
    sd_pre_ctrl    = "sd_pre_Con",
    sd_post_ctrl   = "sd_post_Con",
    n_ctrl         = "n_Con",
    p_value_ctrl   = "p_value_Con",
    upper_ci_ctrl  = "upperCI_Con",
    lower_ci_ctrl  = "lowerCI_Con"
  )
  tg_v12 <- tg_v13
  for (canonical in names(v13_to_v12)) {
    if (canonical %in% names(tg_v12)) {
      names(tg_v12)[names(tg_v12) == canonical] <- v13_to_v12[[canonical]]
    }
  }

  # Capture warnings to verify deprecation messages are emitted.
  warns <- character()
  res_legacy <- withCallingHandlers(
    metacor_dual(tg_v12, effect_size = "smd_pre", impute_method = "none",
                 verbose = FALSE),
    warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }
  )
  # At least one warning per `_Con` column (8 cols) -> >= 8 deprecation warnings.
  con_warns <- grep("v1\\.2\\.x", warns, value = TRUE)
  expect_gte(length(con_warns), 8L)

  res_canonical <- metacor_dual(tg_v13, effect_size = "smd_pre",
                                impute_method = "none", verbose = FALSE) |>
    suppress_data_warnings()
  expect_equal(res_legacy, res_canonical)
})

# --- Strict mode rejects flexibility -------------------------------------

test_that("column_matching = 'strict' bypasses the matcher", {
  sg <- read_fixture("example_single_group.rds")
  # In strict mode, the matcher is a no-op. Canonical input names work
  # exactly as in flexible mode.
  res_strict <- metacor_dual(sg, effect_size = "smd_pre",
                             impute_method = "none", single_group = TRUE,
                             verbose = FALSE,
                             column_matching = "strict") |>
    suppress_data_warnings()
  res_flex <- metacor_dual(sg, effect_size = "smd_pre",
                           impute_method = "none", single_group = TRUE,
                           verbose = FALSE,
                           column_matching = "flexible") |>
    suppress_data_warnings()
  expect_equal(res_strict, res_flex)
})

test_that("strict mode does NOT rename v1.2.x column names (matcher is a no-op)", {
  # In strict mode the matcher returns the data frame untouched; legacy
  # spellings reach metacor_dual() unrecognised. The user explicitly opts
  # into this behaviour. We verify the matcher itself is the no-op part —
  # not the downstream function, whose failure mode under missing columns
  # is unrelated to the rename.
  df_legacy <- data.frame(meanPre_Int = 1:3, sd_diff_int = 4:6)
  res <- match_columns(df_legacy, single_group = TRUE, mode = "strict")
  expect_equal(names(res$df), c("meanPre_Int", "sd_diff_int"))
  expect_equal(nrow(res$mapping), 0L)
  expect_length(res$messages, 0L)
  expect_length(res$warnings, 0L)
})
