# Golden tests — single-group designs
#
# Locks down metacor_dual() output for the canonical example dataset
# (paper/metacor analysis Paper/Example Metacor Paper.xlsx, sheet "Sheet1 (2)",
# renamed to v1.3 canonical schema) under every (effect_size x impute_method)
# combination and a few extras.
#
# These tests are the regression net for any future change to metacor_dual().
# If a test fails, either the change was unintended (fix the code) or the
# change was deliberate (regenerate fixtures via
# tools/build_golden_fixtures.R, document in NEWS.md).

sg <- read_fixture("example_single_group.rds")

base_args <- list(
  digits             = 3,
  add_to_df          = TRUE,
  derive_from        = "both",
  apply_hedges       = TRUE,
  mean_differences   = FALSE,
  verbose            = TRUE,
  report_imputations = FALSE,
  single_group       = TRUE
)

run_with <- function(...) {
  args <- modifyList(base_args, list(...))
  args$df <- sg
  do.call(run_capturing, args)
}

test_that("single-group smd_pre x impute_method grid matches golden", {
  for (im in c("none", "direct", "mean", "cv")) {
    label <- sprintf("sg_smd_pre_%s", im)
    expect_matches_golden(label,
      run_with(effect_size = "smd_pre", impute_method = im))
  }
})

test_that("single-group smd_change x impute_method grid matches golden", {
  for (im in c("none", "direct", "mean", "cv")) {
    label <- sprintf("sg_smd_change_%s", im)
    expect_matches_golden(label,
      run_with(effect_size = "smd_change", impute_method = im))
  }
})

test_that("single-group custom sd_change_int + cv matches golden", {
  expect_matches_golden(
    "sg_smd_pre_cv_custom",
    run_with(effect_size = "smd_pre", impute_method = "cv",
             custom_sd_change_int = list(list(row = 7, value = 0.5)))
  )
})

test_that("single-group smd_change + mean_differences=TRUE + cv matches golden", {
  expect_matches_golden(
    "sg_smd_change_cv_meandiffs",
    run_with(effect_size = "smd_change", impute_method = "cv",
             mean_differences = TRUE)
  )
})

test_that("single-group digits=NULL preserves full precision (golden)", {
  expect_matches_golden(
    "sg_smd_pre_cv_no_round",
    run_with(effect_size = "smd_pre", impute_method = "cv",
             digits = NULL, verbose = FALSE)
  )
})

test_that("single-group apply_hedges=FALSE matches golden", {
  expect_matches_golden(
    "sg_smd_pre_cv_no_hedges",
    run_with(effect_size = "smd_pre", impute_method = "cv",
             apply_hedges = FALSE, verbose = FALSE)
  )
})
