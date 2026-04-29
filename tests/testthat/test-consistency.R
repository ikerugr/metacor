# Golden tests — check_metacor_consistency()

test_that("consistency check on single-group output matches golden", {
  sg <- read_fixture("example_single_group.rds")
  base <- metacor_dual(
    sg, digits = 4, add_to_df = TRUE, derive_from = "both",
    apply_hedges = TRUE, effect_size = "smd_pre", mean_differences = TRUE,
    impute_method = "cv", verbose = FALSE, report_imputations = FALSE,
    single_group = TRUE
  ) |> suppressWarnings()
  res <- check_metacor_consistency(base, interpret = TRUE)
  expect_equal(res, load_golden("consistency_sg")$result)
})

test_that("consistency check on two-group output matches golden", {
  tg <- read_fixture("example_two_group.rds")
  base <- metacor_dual(
    tg, digits = 4, add_to_df = TRUE, derive_from = "both",
    apply_hedges = TRUE, effect_size = "smd_pre", mean_differences = TRUE,
    impute_method = "cv", verbose = FALSE, report_imputations = FALSE,
    single_group = FALSE
  ) |> suppressWarnings()
  res <- check_metacor_consistency(base, interpret = TRUE)
  expect_equal(res, load_golden("consistency_tg")$result)
})
