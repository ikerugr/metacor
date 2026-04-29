#' Check internal consistency of meta-analytic summary data
#'
#' This function takes the output of \code{metacor_dual()} and performs
#' a set of internal consistency checks (p-values, confidence intervals,
#' SD of change scores vs. feasible range, and extreme correlations).
#' Optionally, it also adds a human-readable interpretation of the
#' detected issues for each study.
#'
#' @param df A data frame, typically the output of \code{metacor_dual()}.
#' @param tolerance_p Numeric. Maximum acceptable absolute difference between
#'   reported and reconstructed p-values.
#' @param tolerance_CI Numeric. Maximum acceptable absolute difference between
#'   reported and reconstructed confidence interval limits.
#' @param r_extreme Numeric. Threshold above which correlations (in absolute
#'   value) are flagged as extreme.
#' @param interpret Logical. If \code{TRUE}, add character columns
#'   \code{summary_int} (and \code{summary_ctrl}, if applicable) with a
#'   brief narrative interpretation of the flags for each study.
#'
#' @return The same data frame \code{df} with additional logical flag columns
#'   (e.g., \code{flag_p_mismatch_int}, \code{flag_CI_mismatch_int},
#'   \code{flag_sd_change_out_of_range_int}, \code{flag_r_extreme_int}, and
#'   their `_ctrl` counterparts when applicable), and, if
#'   \code{interpret = TRUE}, one or two character columns with textual
#'   summaries.
#' @export
#'
#' @examples
#' \dontrun{
#'   res <- metacor_dual(dat, mean_differences = TRUE)
#'   res_checked <- check_metacor_consistency(res, interpret = TRUE)
#' }
check_metacor_consistency <- function(df,
                                      tolerance_p  = 0.01,
                                      tolerance_CI = 0.10,
                                      r_extreme    = 0.99,
                                      interpret    = FALSE) {

  n <- nrow(df)
  if (n == 0L) return(df)

  ## ------------------------------------------------------------------
  ## Availability of required columns (canonical v1.3 schema)
  ## ------------------------------------------------------------------
  has_int_MD <- all(c("mean_diff_int", "se_mean_diff_int", "n_int") %in% names(df))
  has_int_CI <- all(c("lower_ci_int", "upper_ci_int") %in% names(df))
  has_int_SD <- all(c("sd_pre_int", "sd_post_int", "sd_change_int") %in% names(df))
  has_int_r  <- "r_int" %in% names(df)
  has_int_p  <- "p_value_int" %in% names(df)

  has_ctrl    <- "n_ctrl" %in% names(df)
  has_ctrl_MD <- has_ctrl && all(c("mean_diff_ctrl", "se_mean_diff_ctrl", "n_ctrl") %in% names(df))
  has_ctrl_CI <- has_ctrl && all(c("lower_ci_ctrl", "upper_ci_ctrl") %in% names(df))
  has_ctrl_SD <- has_ctrl && all(c("sd_pre_ctrl", "sd_post_ctrl", "sd_change_ctrl") %in% names(df))
  has_ctrl_r  <- has_ctrl && "r_ctrl" %in% names(df)
  has_ctrl_p  <- has_ctrl && "p_value_ctrl" %in% names(df)

  ## ------------------------------------------------------------------
  ## Flag initialisation
  ## ------------------------------------------------------------------
  df$flag_p_mismatch_int             <- FALSE
  df$flag_CI_mismatch_int            <- FALSE
  df$flag_sd_change_out_of_range_int <- FALSE
  df$flag_r_extreme_int              <- FALSE

  if (has_ctrl) {
    df$flag_p_mismatch_ctrl             <- FALSE
    df$flag_CI_mismatch_ctrl            <- FALSE
    df$flag_sd_change_out_of_range_ctrl <- FALSE
    df$flag_r_extreme_ctrl              <- FALSE
  }

  ## ------------------------------------------------------------------
  ## 1) Flag computation
  ## ------------------------------------------------------------------
  for (i in seq_len(n)) {

    ## ------------------------- ##
    ##  INTERVENTION             ##
    ## ------------------------- ##

    # 1) Reconstructed p-value vs reported p-value
    if (has_int_MD && has_int_p &&
        !any(is.na(c(df$mean_diff_int[i], df$se_mean_diff_int[i], df$n_int[i]))) &&
        df$n_int[i] > 1) {

      t_calc <- df$mean_diff_int[i] / df$se_mean_diff_int[i]
      p_calc <- 2 * (1 - stats::pt(abs(t_calc), df = df$n_int[i] - 1))

      if (!is.na(df$p_value_int[i]) &&
          abs(df$p_value_int[i] - p_calc) > tolerance_p) {
        df$flag_p_mismatch_int[i] <- TRUE
      }

      # 2) Reconstructed CI vs reported CI
      if (has_int_CI &&
          !any(is.na(c(df$lower_ci_int[i], df$upper_ci_int[i])))) {

        t_crit     <- stats::qt(0.975, df = df$n_int[i] - 1)
        lower_calc <- df$mean_diff_int[i] - t_crit * df$se_mean_diff_int[i]
        upper_calc <- df$mean_diff_int[i] + t_crit * df$se_mean_diff_int[i]

        dif_lower <- abs(df$lower_ci_int[i] - lower_calc)
        dif_upper <- abs(df$upper_ci_int[i] - upper_calc)
        if (max(dif_lower, dif_upper) > tolerance_CI) {
          df$flag_CI_mismatch_int[i] <- TRUE
        }
      }
    }

    # 3) Feasible range of sd_change given sd_pre and sd_post
    if (has_int_SD &&
        !any(is.na(c(df$sd_pre_int[i], df$sd_post_int[i])))) {

      preI  <- df$sd_pre_int[i]
      postI <- df$sd_post_int[i]

      sd_minI <- sqrt(pmax(preI^2 + postI^2 - 2 * 0.9999  * preI * postI, 0))
      sd_maxI <- sqrt(pmax(preI^2 + postI^2 - 2 * (-0.9999) * preI * postI, 0))

      if (!is.na(df$sd_change_int[i]) &&
          (df$sd_change_int[i] < sd_minI || df$sd_change_int[i] > sd_maxI)) {
        df$flag_sd_change_out_of_range_int[i] <- TRUE
      }
    }

    # 4) Extreme r
    if (has_int_r &&
        !is.na(df$r_int[i]) && abs(df$r_int[i]) > r_extreme) {
      df$flag_r_extreme_int[i] <- TRUE
    }

    ## ------------------------- ##
    ##  CONTROL (if present)     ##
    ## ------------------------- ##
    if (has_ctrl) {

      # 1) Reconstructed p-value vs reported p-value
      if (has_ctrl_MD && has_ctrl_p &&
          !any(is.na(c(df$mean_diff_ctrl[i], df$se_mean_diff_ctrl[i], df$n_ctrl[i]))) &&
          df$n_ctrl[i] > 1) {

        t_calc <- df$mean_diff_ctrl[i] / df$se_mean_diff_ctrl[i]
        p_calc <- 2 * (1 - stats::pt(abs(t_calc), df = df$n_ctrl[i] - 1))

        if (!is.na(df$p_value_ctrl[i]) &&
            abs(df$p_value_ctrl[i] - p_calc) > tolerance_p) {
          df$flag_p_mismatch_ctrl[i] <- TRUE
        }

        # 2) Reconstructed CI vs reported CI
        if (has_ctrl_CI &&
            !any(is.na(c(df$lower_ci_ctrl[i], df$upper_ci_ctrl[i])))) {

          t_crit     <- stats::qt(0.975, df = df$n_ctrl[i] - 1)
          lower_calc <- df$mean_diff_ctrl[i] - t_crit * df$se_mean_diff_ctrl[i]
          upper_calc <- df$mean_diff_ctrl[i] + t_crit * df$se_mean_diff_ctrl[i]

          dif_lower <- abs(df$lower_ci_ctrl[i] - lower_calc)
          dif_upper <- abs(df$upper_ci_ctrl[i] - upper_calc)
          if (max(dif_lower, dif_upper) > tolerance_CI) {
            df$flag_CI_mismatch_ctrl[i] <- TRUE
          }
        }
      }

      # 3) Feasible range of sd_change given sd_pre and sd_post
      if (has_ctrl_SD &&
          !any(is.na(c(df$sd_pre_ctrl[i], df$sd_post_ctrl[i])))) {

        preC  <- df$sd_pre_ctrl[i]
        postC <- df$sd_post_ctrl[i]

        sd_minC <- sqrt(pmax(preC^2 + postC^2 - 2 * 0.9999  * preC * postC, 0))
        sd_maxC <- sqrt(pmax(preC^2 + postC^2 - 2 * (-0.9999) * preC * postC, 0))

        if (!is.na(df$sd_change_ctrl[i]) &&
            (df$sd_change_ctrl[i] < sd_minC || df$sd_change_ctrl[i] > sd_maxC)) {
          df$flag_sd_change_out_of_range_ctrl[i] <- TRUE
        }
      }

      # 4) Extreme r
      if (has_ctrl_r &&
          !is.na(df$r_ctrl[i]) && abs(df$r_ctrl[i]) > r_extreme) {
        df$flag_r_extreme_ctrl[i] <- TRUE
      }
    }
  }

  ## ------------------------------------------------------------------
  ## 2) Narrative interpretation (optional)
  ## ------------------------------------------------------------------
  if (isTRUE(interpret)) {

    summary_int  <- character(n)
    summary_ctrl <- if (has_ctrl) character(n) else NULL

    for (i in seq_len(n)) {

      ## -------- INTERVENTION --------
      msgs <- character(0)

      if (isTRUE(df$flag_p_mismatch_int[i])) {
        msgs <- c(
          msgs,
          "Reconstructed p-value for the intervention group does not match the reported p-value (beyond the specified tolerance)."
        )
      }

      if (isTRUE(df$flag_CI_mismatch_int[i])) {
        msgs <- c(
          msgs,
          "Reconstructed 95% confidence interval for the intervention group does not match the reported limits."
        )
      }

      if (isTRUE(df$flag_sd_change_out_of_range_int[i])) {

        if (has_int_SD) {
          pre  <- df$sd_pre_int[i]
          post <- df$sd_post_int[i]
          sdd  <- df$sd_change_int[i]

          if (!any(is.na(c(pre, post, sdd)))) {
            sd_min <- sqrt(pmax(pre^2 + post^2 - 2 * 0.9999  * pre * post, 0))
            sd_max <- sqrt(pmax(pre^2 + post^2 - 2 * (-0.9999) * pre * post, 0))

            if (sdd < sd_min) {
              msgs <- c(
                msgs,
                sprintf(
                  "The SD of the pre-post change in the intervention group (%.3f) is smaller than the minimum feasible value (approx. %.3f) given sd_pre = %.3f and sd_post = %.3f, implying an impossible correlation (r > 1).",
                  sdd, sd_min, pre, post
                )
              )
            } else if (sdd > sd_max) {
              msgs <- c(
                msgs,
                sprintf(
                  "The SD of the pre-post change in the intervention group (%.3f) is larger than the maximum feasible value (approx. %.3f) given sd_pre = %.3f and sd_post = %.3f, implying an impossible correlation (r < -1).",
                  sdd, sd_max, pre, post
                )
              )
            } else {
              msgs <- c(
                msgs,
                "The SD of the pre-post change in the intervention group is incompatible with the reported pre and post SDs (implies a correlation outside [-1, 1])."
              )
            }
          } else {
            msgs <- c(
              msgs,
              "The SD of the pre-post change in the intervention group is incompatible with the reported pre and post SDs (implies a correlation outside [-1, 1])."
            )
          }
        } else {
          msgs <- c(
            msgs,
            "The SD of the pre-post change in the intervention group appears incompatible with the reported data."
          )
        }
      }

      if (isTRUE(df$flag_r_extreme_int[i])) {
        if (has_int_r && !is.na(df$r_int[i])) {
          r_val <- df$r_int[i]
          msgs <- c(
            msgs,
            sprintf(
              "The pre-post correlation in the intervention group is extremely high in absolute value (r = %.3f), suggesting almost perfect tracking between measurements. This may be genuine but should be checked against the original report.",
              r_val
            )
          )
        } else {
          msgs <- c(
            msgs,
            "The pre-post correlation in the intervention group is extremely high in absolute value (close to +/-1). This may be genuine but should be checked against the original report."
          )
        }
      }

      if (length(msgs) == 0) {
        summary_int[i] <- "No internal inconsistencies detected for the intervention group based on the current checks."
      } else {
        summary_int[i] <- paste(msgs, collapse = " ")
      }

      ## -------- CONTROL (if present) --------
      if (has_ctrl) {
        msgs <- character(0)

        if (isTRUE(df$flag_p_mismatch_ctrl[i])) {
          msgs <- c(
            msgs,
            "Reconstructed p-value for the control group does not match the reported p-value (beyond the specified tolerance)."
          )
        }

        if (isTRUE(df$flag_CI_mismatch_ctrl[i])) {
          msgs <- c(
            msgs,
            "Reconstructed 95% confidence interval for the control group does not match the reported limits."
          )
        }

        if (isTRUE(df$flag_sd_change_out_of_range_ctrl[i])) {

          if (has_ctrl_SD) {
            pre  <- df$sd_pre_ctrl[i]
            post <- df$sd_post_ctrl[i]
            sdd  <- df$sd_change_ctrl[i]

            if (!any(is.na(c(pre, post, sdd)))) {
              sd_min <- sqrt(pmax(pre^2 + post^2 - 2 * 0.9999  * pre * post, 0))
              sd_max <- sqrt(pmax(pre^2 + post^2 - 2 * (-0.9999) * pre * post, 0))

              if (sdd < sd_min) {
                msgs <- c(
                  msgs,
                  sprintf(
                    "The SD of the pre-post change in the control group (%.3f) is smaller than the minimum feasible value (approx. %.3f) given sd_pre = %.3f and sd_post = %.3f, implying an impossible correlation (r > 1).",
                    sdd, sd_min, pre, post
                  )
                )
              } else if (sdd > sd_max) {
                msgs <- c(
                  msgs,
                  sprintf(
                    "The SD of the pre-post change in the control group (%.3f) is larger than the maximum feasible value (approx. %.3f) given sd_pre = %.3f and sd_post = %.3f, implying an impossible correlation (r < -1).",
                    sdd, sd_max, pre, post
                  )
                )
              } else {
                msgs <- c(
                  msgs,
                  "The SD of the pre-post change in the control group is incompatible with the reported pre and post SDs (implies a correlation outside [-1, 1])."
                )
              }
            } else {
              msgs <- c(
                msgs,
                "The SD of the pre-post change in the control group is incompatible with the reported pre and post SDs (implies a correlation outside [-1, 1])."
              )
            }
          } else {
            msgs <- c(
              msgs,
              "The SD of the pre-post change in the control group appears incompatible with the reported data."
            )
          }
        }

        if (isTRUE(df$flag_r_extreme_ctrl[i])) {
          if (has_ctrl_r && !is.na(df$r_ctrl[i])) {
            r_val <- df$r_ctrl[i]
            msgs <- c(
              msgs,
              sprintf(
                "The pre-post correlation in the control group is extremely high in absolute value (r = %.3f), suggesting almost perfect tracking between measurements. This may be genuine but should be checked against the original report.",
                r_val
              )
            )
          } else {
            msgs <- c(
              msgs,
              "The pre-post correlation in the control group is extremely high in absolute value (close to +/-1). This may be genuine but should be checked against the original report."
            )
          }
        }

        if (length(msgs) == 0) {
          summary_ctrl[i] <- "No internal inconsistencies detected for the control group based on the current checks."
        } else {
          summary_ctrl[i] <- paste(msgs, collapse = " ")
        }
      } # has_ctrl
    } # for i

    df$summary_int <- summary_int
    if (has_ctrl) {
      df$summary_ctrl <- summary_ctrl
    }
  }

  df
}
