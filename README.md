
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metacor

<!-- badges: start -->
<!-- badges: end -->

metacor is an R package for advanced meta-analysis of pre-post studies,
with or without a control group. It provides flexible effect size
calculation, robust imputation strategies for missing SDs or
correlations, and reproducible reporting.

## Installation

You can install the development version of metacor from GitHub

``` r
# Install the remotes package if needed:
# install.packages("remotes")

remotes::install_github("https://github.com/ikerugr/metacor")
```

## Example

Suppose you have a data frame of pre-post means, SDs, and sample sizes.
Here’s a minimal example with fake data:

``` r
library(metacor)

# Example dataset (add your real studies for actual analysis)
df <- data.frame(
  study_name = c("Study1", "Study2", "Study3", "Study4","Study5", "Study6", "Study7", "Study8", "Study9"),
  p_value_Int = c(1.038814e-07, NA, NA, NA, NA, 2.100000e-02, NA, NA, NA),
  n_Int = c(10, 10, 10, 10, 15, 15, 10, 10, 10),
  meanPre_Int = c(8.17, 10.09, 10.18, 9.85, 9.51,7.70, 10.00,  11.53, 11.20),
  meanPost_Int = c(10.12, 12.50, 12.56,10.41, 10.88, 9.20, 10.80,13.42,12.00),
  sd_pre_Int = c(1.83,0.67,0.66,0.90,0.62, 0.90, 0.70, 0.60, 1.90),
  sd_post_Int = c(1.85, 0.72, 0.97, 0.67, 0.76, 1.10, 0.70,0.80,1.80),
  upperCI_Int = c(NA, NA,NA, NA,NA, NA,NA, NA, NA),
  lowerCI_Int = c(NA, NA,NA, NA,NA, NA,NA, NA, NA))

results <- metacor_dual(df,
                        digits = 3,
                        method = "both",
                        apply_hedges = TRUE,
                        add_to_df = TRUE,
                        SMD_method = "SMDpre",
                        MeanDifferences = TRUE,
                        impute_method = "cv",
                        verbose = TRUE,
                        report_imputations = TRUE,
                        custom_sd_diff_int = NULL,
                        custom_sd_diff_con = NULL,
                        single_group = TRUE)
#> Warning in metacor_dual(df, digits = 3, method = "both", apply_hedges = TRUE, :
#> No real SD diff values available to impute (con).
#> Imputed sd_diff_int at row 2 using 'cv' (0.8494): 2.0470
#> Warning in metacor_dual(df, digits = 3, method = "both", apply_hedges = TRUE, : Row 2: Imputed sd_diff_int = 2.0470 gives r_int = -3.3405 (outside [-0.9999, 0.9999]).
#> → Suggested sd_diff_int range: [0.0510, 1.3900].
#> → r_int not assigned.
#> Imputed sd_diff_int at row 3 using 'cv' (0.8494): 2.0215
#> Warning in metacor_dual(df, digits = 3, method = "both", apply_hedges = TRUE, : Row 3: Imputed sd_diff_int = 2.0215 gives r_int = -2.1166 (outside [-0.9999, 0.9999]).
#> → Suggested sd_diff_int range: [0.3102, 1.6300].
#> → r_int not assigned.
#> Imputed sd_diff_int at row 4 using 'cv' (0.8494): 0.4757
#> Imputed sd_diff_int at row 5 using 'cv' (0.8494): 1.1636
#> Imputed sd_diff_int at row 7 using 'cv' (0.8494): 0.6795
#> Imputed sd_diff_int at row 8 using 'cv' (0.8494): 1.6053
#> Warning in metacor_dual(df, digits = 3, method = "both", apply_hedges = TRUE, : Row 8: Imputed sd_diff_int = 1.6053 gives r_int = -1.6428 (outside [-0.9999, 0.9999]).
#> → Suggested sd_diff_int range: [0.2002, 1.4000].
#> → r_int not assigned.
#> Imputed sd_diff_int at row 9 using 'cv' (0.8494): 0.6795
```

By default, a detailed Word report (imputation_report.docx) is
generated, describing all imputations made. You can adjust arguments for
imputation method, effect size, verbosity, and more.

## Features

- Calculates multiple effect size metrics: SMDpre, SMDchange,
  ScMDpooled, ScMDpre.
- Handles both single-group and two-group (control vs intervention)
  meta-analyses.
- Flexible imputation: mean, direct, coefficient of variation, manual.
- Generates a transparent, reproducible report for your systematic
  review or meta-analysis.

## More information

vignette(“metacor-intro”)

## Author

Iker J. Bautista (2025) Chichester University

This README was generated with readme.Rmd.
