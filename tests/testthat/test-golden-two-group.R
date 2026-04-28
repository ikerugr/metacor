# Golden tests — two-group (intervention + control) designs
#
# Uses the synthetic two-group fixture (see tests/testthat/_data/README.md).
# Locks down metacor_dual() output for every effect-size x {none, cv}
# imputation combination, plus a custom-imputation case.

tg <- read_fixture("example_two_group.rds")

base_args <- list(
  digits          = 4,
  add_to_df       = TRUE,
  method          = "both",
  apply_hedges    = TRUE,
  MeanDifferences = TRUE,
  verbose         = FALSE,
  report_imputations = FALSE,
  single_group    = FALSE
)

run_with <- function(...) {
  args <- modifyList(base_args, list(...))
  args$df <- tg
  do.call(run_capturing, args)
}

test_that("two-group SMDpre / SMDchange / ScMDpooled / ScMDpre x {none, cv} match golden", {
  for (es in c("SMDpre", "SMDchange", "ScMDpooled", "ScMDpre")) {
    for (im in c("none", "cv")) {
      label <- sprintf("tg_%s_%s", tolower(es), im)
      expect_matches_golden(label,
        run_with(SMD_method = es, impute_method = im))
    }
  }
})

test_that("two-group custom sd_diff in both groups matches golden", {
  expect_matches_golden(
    "tg_smdchange_custom_both",
    run_with(SMD_method = "SMDchange", impute_method = "none",
             MeanDifferences = FALSE,
             custom_sd_diff_int = list(list(row = 3, value = 0.6),
                                       list(row = 6, value = 5.0)),
             custom_sd_diff_con = list(list(row = 3, value = 0.5),
                                       list(row = 6, value = 4.5)))
  )
})
