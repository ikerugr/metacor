# Golden tests — two-group (intervention + control) designs
#
# Uses the synthetic two-group fixture (see tests/testthat/_data/README.md).
# Locks down metacor_dual() output for every effect_size x {none, cv}
# imputation combination, plus a custom-imputation case.

tg <- read_fixture("example_two_group.rds")

base_args <- list(
  digits             = 4,
  add_to_df          = TRUE,
  derive_from        = "both",
  apply_hedges       = TRUE,
  mean_differences   = TRUE,
  verbose            = FALSE,
  report_imputations = FALSE,
  single_group       = FALSE
)

run_with <- function(...) {
  args <- modifyList(base_args, list(...))
  args$df <- tg
  do.call(run_capturing, args)
}

test_that("two-group {smd_pre, smd_change, smd_pooled, smd_diff_groups} x {none, cv} match golden", {
  for (es in c("smd_pre", "smd_change", "smd_pooled", "smd_diff_groups")) {
    for (im in c("none", "cv")) {
      label <- sprintf("tg_%s_%s", es, im)
      expect_matches_golden(label,
        run_with(effect_size = es, impute_method = im))
    }
  }
})

test_that("two-group custom sd_change in both groups matches golden", {
  expect_matches_golden(
    "tg_smd_change_custom_both",
    run_with(effect_size = "smd_change", impute_method = "none",
             mean_differences = FALSE,
             custom_sd_change_int = list(list(row = 3, value = 0.6),
                                         list(row = 6, value = 5.0)),
             custom_sd_change_ctrl = list(list(row = 3, value = 0.5),
                                          list(row = 6, value = 4.5)))
  )
})
