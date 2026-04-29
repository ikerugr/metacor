# Helpers for golden / fixture-based testing.
#
# Conventions:
#   - Input CSVs live in tests/testthat/_data/
#   - Captured outputs live in tests/testthat/_golden/<label>.rds
#   - Each .rds is a list(label = chr, result = data.frame,
#                         messages = chr, warnings = chr)
#
# To regenerate fixtures (after a deliberate behaviour change):
#   Rscript tools/build_golden_fixtures.R

# testthat sets the working directory to tests/testthat/ when running tests,
# so relative paths from here are simple.
GOLDEN_DIR <- "_golden"
DATA_DIR   <- "_data"

read_fixture <- function(name) {
  # Inputs are stored as .rds to preserve column types exactly.
  path <- file.path(DATA_DIR, name)
  readRDS(path)
}

load_golden <- function(label) {
  readRDS(file.path(GOLDEN_DIR, paste0(label, ".rds")))
}

# Run metacor_dual() while capturing messages and warnings, like the
# fixture builder does. Returns a list with the same shape as the .rds.
run_capturing <- function(...) {
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
  list(result = res, messages = msgs, warnings = warns)
}

# Compare a fresh run against a stored golden fixture.
# Strict on the result data frame; for messages/warnings we compare the
# *set* of strings (order-insensitive) since R's condition delivery order
# can vary across versions.
expect_matches_golden <- function(label, run) {
  golden <- load_golden(label)
  testthat::expect_equal(
    run$result, golden$result,
    info = paste("result mismatch for golden:", label)
  )
  testthat::expect_setequal(run$messages, golden$messages)
  testthat::expect_setequal(run$warnings, golden$warnings)
}

# Run an expression, suppressing only data-related warnings while letting
# `lifecycle_warning_deprecated` warnings propagate so testthat can match
# them via `lifecycle::expect_deprecated()`.
suppress_data_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (!inherits(w, "lifecycle_warning_deprecated")) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
