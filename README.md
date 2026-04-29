
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metacor

<!-- badges: start -->
<!-- badges: end -->

`metacor` is an R package for meta-analyses of pre–post studies, with or
without a control group. It calculates effect sizes (standardised mean
differences and mean differences) and imputes the two summary statistics
that primary studies most often fail to report — the **standard
deviation of the change scores** (`sd_change`) and the **pre–post
correlation coefficient** (`r`) — so the variance of the effect size can
still be estimated.

## Installation

The released version is on CRAN:

``` r
install.packages("metacor")
```

The development version is on GitHub:

``` r
# install.packages("remotes")
remotes::install_github("ikerugr/metacor")
```

## Quick example (single-group pre–post)

``` r
library(metacor)

df <- data.frame(
  study_name    = paste0("Study", 1:9),
  p_value_int   = c(1.04e-07, NA, NA, NA, NA, 0.021, NA, NA, NA),
  n_int         = c(10, 10, 10, 10, 15, 15, 10, 10, 10),
  mean_pre_int  = c(8.17, 10.09, 10.18, 9.85, 9.51, 7.70, 10.00, 11.53, 11.20),
  mean_post_int = c(10.12, 12.50, 12.56, 10.41, 10.88, 9.20, 10.80, 13.42, 12.00),
  sd_pre_int    = c(1.83, 0.67, 0.66, 0.90, 0.62, 0.90, 0.70, 0.60, 1.90),
  sd_post_int   = c(1.85, 0.72, 0.97, 0.67, 0.76, 1.10, 0.70, 0.80, 1.80),
  upper_ci_int  = NA_real_,
  lower_ci_int  = NA_real_
)

result <- metacor_dual(
  df,
  single_group  = TRUE,
  effect_size   = "smd_pre",
  impute_method = "cv",
  digits        = 3,
  verbose       = FALSE
) |> suppressWarnings()

result[, c("study_name", "r_int", "sd_change_int",
           "smd_pre_int", "var_smd_pre_int")]
#>   study_name r_int sd_change_int smd_pre_int var_smd_pre_int
#> 1     Study1 0.976         0.407       0.974           0.076
#> 2     Study2 0.465         0.720       3.289           0.923
#> 3     Study3 0.680         0.711       3.297           0.881
#> 4     Study4    NA         0.167       0.569              NA
#> 5     Study5 0.843         0.409       2.089           0.209
#> 6     Study6    NA         2.235       1.576              NA
#> 7     Study7 0.942         0.239       1.045           0.094
#> 8     Study8 0.710         0.565       2.880           0.682
#> 9     Study9 0.993         0.239       0.385           0.013
```

Set `report_imputations = TRUE` to generate `imputation_report.docx`
describing every imputation performed (study, group, value, method,
suggested feasible range) for inclusion as supplementary material.

## Features

- Four effect-size families: `smd_pre`, `smd_change`, `smd_pooled`,
  `smd_diff_groups`.
- Single-group pre–post **and** intervention-vs-control designs.
- Imputation strategies for missing `sd_change`: `none`, `direct`,
  `mean`, `cv` (recommended), and per-study manual overrides.
- Flexible column-name matching: accepts canonical names, v1.2.x legacy
  names, and a closed dictionary of synonyms (`baseline_mean`,
  `treatment`, `placebo`, …) so users don’t have to reformat their data
  manually.
- `check_metacor_consistency()` flags p-value / CI / `sd_change`
  feasibility / extreme-`r` issues and provides per-study narrative
  summaries.
- Transparent, reproducible Word imputation report.

## Documentation

``` r
vignette("metacor-intro",       package = "metacor")
vignette("migrating-from-1.2",  package = "metacor")
```

## Author

Iker J. Bautista — University of Chichester (2025).

`metacor` was developed alongside an applied paper describing the
imputation methodology; see the package help pages and vignettes for
full citations.
