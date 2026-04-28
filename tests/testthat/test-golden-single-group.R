# Golden tests — single-group designs
#
# Locks down metacor_dual() output for the canonical example dataset
# (paper/metacor analysis Paper/Example Metacor Paper.xlsx, sheet "Sheet1 (2)")
# under every (effect_size x impute_method) combination and a few extras.
#
# These tests are the safety net for the v1.2.1 -> v1.3.0 rename. If a test
# fails after a refactor, either the change was unintended (fix the code)
# or the change was deliberate (regenerate fixtures via
# tools/build_golden_fixtures.R, document in NEWS.md).

sg <- read_fixture("example_single_group.rds")

base_args <- list(
  df              = quote(sg),
  digits          = 3,
  add_to_df       = TRUE,
  method          = "both",
  apply_hedges    = TRUE,
  MeanDifferences = FALSE,
  verbose         = TRUE,
  report_imputations = FALSE,
  single_group    = TRUE
)

run_with <- function(...) {
  args <- modifyList(base_args, list(...))
  args$df <- sg
  do.call(run_capturing, args)
}

test_that("single-group SMDpre x impute_method grid matches golden", {
  for (im in c("none", "direct", "mean", "cv")) {
    label <- sprintf("sg_smdpre_%s", im)
    expect_matches_golden(label,
      run_with(SMD_method = "SMDpre", impute_method = im))
  }
})

test_that("single-group SMDchange x impute_method grid matches golden", {
  for (im in c("none", "direct", "mean", "cv")) {
    label <- sprintf("sg_smdchange_%s", im)
    expect_matches_golden(label,
      run_with(SMD_method = "SMDchange", impute_method = im))
  }
})

test_that("single-group custom sd_diff_int + cv matches golden", {
  expect_matches_golden(
    "sg_smdpre_cv_custom",
    run_with(SMD_method = "SMDpre", impute_method = "cv",
             custom_sd_diff_int = list(list(row = 7, value = 0.5)))
  )
})

test_that("single-group SMDchange + MeanDifferences=TRUE + cv matches golden", {
  expect_matches_golden(
    "sg_smdchange_cv_meandiffs",
    run_with(SMD_method = "SMDchange", impute_method = "cv",
             MeanDifferences = TRUE)
  )
})

test_that("single-group digits=NULL preserves full precision (golden)", {
  expect_matches_golden(
    "sg_smdpre_cv_no_round",
    run_with(SMD_method = "SMDpre", impute_method = "cv",
             digits = NULL, verbose = FALSE)
  )
})

test_that("single-group apply_hedges=FALSE matches golden", {
  expect_matches_golden(
    "sg_smdpre_cv_no_hedges",
    run_with(SMD_method = "SMDpre", impute_method = "cv",
             apply_hedges = FALSE, verbose = FALSE)
  )
})
