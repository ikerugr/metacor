#' Effect Sizes and Imputation for Meta-Analysis of Pre-Post Studies
#'
#' Calculates effect sizes (i.e., `smd_pre`, `smd_change`, `smd_pooled`,
#' `smd_diff_groups`) and allows for various imputation methods (i.e.,
#' `none`, `cv`, `direct`, `mean`) for missing `sd_change` and correlation
#' coefficients in pre-post meta-analyses, with or without a control group.
#' Generates a detailed imputation report in Word format.
#'
#' Column names in the input data frame are matched flexibly: legacy
#' v1.2.x names (`meanPre_Int`, `sd_diff_int`, `_Con`, ...) and a closed
#' set of common synonyms (`baseline_mean`, `treatment`, `placebo`, ...)
#' are accepted alongside the canonical names. Pass
#' `column_matching = "strict"` to disable this and require canonical
#' names exactly.
#'
#' @param df Data frame with the necessary columns for intervention and
#'   (optionally) control groups. See *Details* for the canonical schema.
#' @param digits Number of decimal places to round results (default: NULL).
#' @param add_to_df Logical. If TRUE, results are added to the original data frame.
#' @param derive_from Method for deriving `sd_change` when not reported.
#'   One of `"both"` (default), `"p_value"`, `"ci"`. Was `method` in v1.2.x.
#' @param apply_hedges Logical. Apply Hedges' g correction? (default: TRUE)
#' @param effect_size Which effect size to compute. One of `"smd_pre"`
#'   (default), `"smd_change"`, `"smd_pooled"`, `"smd_diff_groups"`. Was
#'   `SMD_method` in v1.2.x with values `"SMDpre"`, `"SMDchange"`,
#'   `"ScMDpooled"`, `"ScMDpre"`.
#' @param mean_differences Logical. Calculate mean differences and
#'   variances? (default: FALSE). Was `MeanDifferences` in v1.2.x.
#' @param impute_method Imputation method for missing `sd_change`. One of
#'   `"none"` (default), `"direct"`, `"mean"`, `"cv"`.
#' @param verbose Logical. Print messages during processing? (default: TRUE)
#' @param report_imputations Logical. Generate Word imputation report? (default: FALSE)
#' @param custom_sd_change_int List with elements `row` and `value` for
#'   manual `sd_change` values in the intervention group. Was
#'   `custom_sd_diff_int` in v1.2.x.
#' @param custom_sd_change_ctrl Same for the control group. Was
#'   `custom_sd_diff_con` in v1.2.x.
#' @param single_group Logical. Is the design single-group only? (default: FALSE)
#' @param column_matching `"flexible"` (default) accepts case / separator
#'   variants and a closed set of synonyms; `"strict"` requires the exact
#'   canonical column names.
#' @param method `r lifecycle::badge("deprecated")` Use `derive_from`.
#' @param SMD_method `r lifecycle::badge("deprecated")` Use `effect_size`.
#' @param MeanDifferences `r lifecycle::badge("deprecated")` Use `mean_differences`.
#' @param custom_sd_diff_int `r lifecycle::badge("deprecated")` Use `custom_sd_change_int`.
#' @param custom_sd_diff_con `r lifecycle::badge("deprecated")` Use `custom_sd_change_ctrl`.
#' @return Data frame with calculated variables. Optionally, a Word report
#'   (`imputation_report.docx`) is generated.
#' @export
#' @importFrom stats qt
#' @importFrom stringr str_match str_detect
#' @importFrom officer read_docx body_add_fpar body_add_par fpar ftext fp_text
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @examples
#' df <- data.frame(
#'   study_name   = c("Study1", "Study2", "Study3", "Study4"),
#'   p_value_int  = c(1.04e-07, NA, NA, NA),
#'   n_int        = c(10, 10, 10, 10),
#'   mean_pre_int = c(8.17, 10.09, 10.18, 9.85),
#'   mean_post_int= c(10.12, 12.50, 12.56, 10.41),
#'   sd_pre_int   = c(1.83, 0.67, 0.66, 0.90),
#'   sd_post_int  = c(1.85, 0.72, 0.97, 0.67),
#'   upper_ci_int = c(NA, NA, NA, NA),
#'   lower_ci_int = c(NA, NA, NA, NA)
#' )
#' result <- metacor_dual(df, single_group = TRUE)
#' print(result)
#' @references
#' Higgins, J. P. T., Thomas, J., Chandler, J., Cumpston, M., Li, T., Page, M. J., & Welch, V. A. (Eds.). (2023). Cochrane handbook for systematic reviews of interventions (Version 6.3). Cochrane. https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current
#'
#' Fu, R., Vandermeer, B.W., Shamliyan, T.A., ONeil, M.E., Yazdi, F., Fox, S.H., & Morton, S.C. (2013). Handling Continuous Outcomes in Quantitative Synthesis. Methods Guide for Comparative Effectiveness Reviews. AHRQ Publication No. 13-EHC103-EF. https://effectivehealthcare.ahrq.gov/reports/final.cfm
metacor_dual <- function(df,
                         digits = NULL,
                         add_to_df = TRUE,
                         derive_from = c("both", "p_value", "ci"),
                         apply_hedges = TRUE,
                         effect_size = c("smd_pre", "smd_change",
                                         "smd_pooled", "smd_diff_groups"),
                         mean_differences = FALSE,
                         impute_method = c("none", "direct", "mean", "cv"),
                         verbose = TRUE,
                         report_imputations = FALSE,
                         custom_sd_change_int = NULL,
                         custom_sd_change_ctrl = NULL,
                         single_group = FALSE,
                         column_matching = c("flexible", "strict"),
                         # ---- v1.2.x argument shims (do not use) ----
                         method = lifecycle::deprecated(),
                         SMD_method = lifecycle::deprecated(),
                         MeanDifferences = lifecycle::deprecated(),
                         custom_sd_diff_int = lifecycle::deprecated(),
                         custom_sd_diff_con = lifecycle::deprecated()) {

  # -------------------------------------------------------------------
  # BLOCK: backward compatibility — translate v1.2.x args to v1.3 args
  # -------------------------------------------------------------------
  if (lifecycle::is_present(method)) {
    lifecycle::deprecate_warn(
      "1.3.0", "metacor_dual(method)", "metacor_dual(derive_from)"
    )
    derive_from <- method
  }
  if (lifecycle::is_present(SMD_method)) {
    lifecycle::deprecate_warn(
      "1.3.0", "metacor_dual(SMD_method)", "metacor_dual(effect_size)"
    )
    effect_size <- SMD_method
  }
  if (lifecycle::is_present(MeanDifferences)) {
    lifecycle::deprecate_warn(
      "1.3.0", "metacor_dual(MeanDifferences)", "metacor_dual(mean_differences)"
    )
    mean_differences <- MeanDifferences
  }
  if (lifecycle::is_present(custom_sd_diff_int)) {
    lifecycle::deprecate_warn(
      "1.3.0", "metacor_dual(custom_sd_diff_int)",
      "metacor_dual(custom_sd_change_int)"
    )
    custom_sd_change_int <- custom_sd_diff_int
  }
  if (lifecycle::is_present(custom_sd_diff_con)) {
    lifecycle::deprecate_warn(
      "1.3.0", "metacor_dual(custom_sd_diff_con)",
      "metacor_dual(custom_sd_change_ctrl)"
    )
    custom_sd_change_ctrl <- custom_sd_diff_con
  }

  # Translate v1.2.x effect_size *values* ("SMDpre" -> "smd_pre" etc.)
  # before validating. translate_effect_size_value() emits its own warning
  # if a legacy value is detected.
  effect_size <- translate_effect_size_value(effect_size)

  # -------------------------------------------------------------------
  # BLOCK: argument validation
  # -------------------------------------------------------------------
  derive_from     <- match.arg(derive_from)
  effect_size     <- match.arg(effect_size)
  impute_method   <- match.arg(impute_method)
  column_matching <- match.arg(column_matching)

  # -------------------------------------------------------------------
  # BLOCK: report-generation packages (only if requested)
  # -------------------------------------------------------------------
  if (isTRUE(report_imputations)) {
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Package 'officer' is required for reporting. Please install it.")
    }
    if (!requireNamespace("stringr", quietly = TRUE)) {
      stop("Package 'stringr' is required for reporting. Please install it.")
    }
  }

  # -------------------------------------------------------------------
  # BLOCK: flexible column-name matching
  # Resolves user column names to the canonical schema. After this point
  # the rest of the function uses `mean_pre_int`, `sd_change_int`, etc.
  # -------------------------------------------------------------------
  matched <- match_columns(df, single_group = single_group,
                           mode = column_matching)
  df <- matched$df
  if (isTRUE(verbose)) {
    for (m in matched$messages) message(m)
  }
  for (w in matched$warnings) warning(w, call. = FALSE)

  imp_log <- vector("list", nrow(df))

  # -------------------------------------------------------------------
  # BLOCK: dummy NA columns for the control group, if absent
  # -------------------------------------------------------------------
  control_vars <- c("mean_pre_ctrl", "mean_post_ctrl",
                    "sd_pre_ctrl", "sd_post_ctrl",
                    "upper_ci_ctrl", "lower_ci_ctrl",
                    "n_ctrl", "p_value_ctrl")

  for (var in control_vars) {
    if (!var %in% names(df)) {
      df[[var]] <- NA_real_
    }
  }

  # -------------------------------------------------------------------
  # BLOCK: vector initialisation
  # -------------------------------------------------------------------
  n <- nrow(df)
  r_int  <- rep(NA_real_, n)
  r_ctrl <- rep(NA_real_, n)

  smd_pre_int <- smd_pre_ctrl <-
    var_smd_pre_int <- var_smd_pre_ctrl <-
    se_smd_pre_int  <- se_smd_pre_ctrl  <- rep(NA_real_, n)
  smd_change_int <- smd_change_ctrl <-
    var_smd_change_int <- var_smd_change_ctrl <-
    se_smd_change_int  <- se_smd_change_ctrl  <- rep(NA_real_, n)
  smd_pooled        <- var_smd_pooled        <- se_smd_pooled        <- rep(NA_real_, n)
  smd_diff_groups   <- var_smd_diff_groups   <- se_smd_diff_groups   <- rep(NA_real_, n)

  pct_change_int <- pct_change_ctrl <- rep(NA_real_, n)
  sd_change_int  <- rep(NA_real_, n)
  sd_change_ctrl <- rep(NA_real_, n)
  mean_diff_int <- mean_diff_ctrl <-
    var_mean_diff_int <- var_mean_diff_ctrl <-
    se_mean_diff_int  <- se_mean_diff_ctrl  <- rep(NA_real_, n)

  # -------------------------------------------------------------------
  # FIRST LOOP: derive sd_change (p_value / CI), mean_diff, initial r
  # -------------------------------------------------------------------
  for (i in seq_len(n)) {

    sd_change_p_int  <- sd_change_ci_int  <- NA_real_
    sd_change_p_ctrl <- sd_change_ci_ctrl <- NA_real_

    if (derive_from %in% c("p_value", "both")) {
      if (!is.na(df$p_value_int[i]) && !is.na(df$n_int[i]) && df$n_int[i] > 1 &&
          !is.na(df$mean_pre_int[i]) && !is.na(df$mean_post_int[i])) {
        t_stat_p_int <- stats::qt(df$p_value_int[i] / 2, df$n_int[i] - 1, lower.tail = FALSE)
        if (is.finite(t_stat_p_int) && t_stat_p_int != 0) {
          se_int <- abs((df$mean_pre_int[i] - df$mean_post_int[i]) / t_stat_p_int)
          if (!is.na(se_int) && se_int >= 0 && !is.na(df$n_int[i]) && df$n_int[i] > 0) {
            sd_change_p_int <- se_int / sqrt(1 / df$n_int[i])
          }
        }
      }

      if (!isTRUE(single_group)) {
        if (!is.na(df$p_value_ctrl[i]) && !is.na(df$n_ctrl[i]) && df$n_ctrl[i] > 1 &&
            !is.na(df$mean_pre_ctrl[i]) && !is.na(df$mean_post_ctrl[i])) {
          t_stat_p_ctrl <- stats::qt(df$p_value_ctrl[i] / 2, df$n_ctrl[i] - 1, lower.tail = FALSE)
          if (is.finite(t_stat_p_ctrl) && t_stat_p_ctrl != 0) {
            se_ctrl <- abs((df$mean_pre_ctrl[i] - df$mean_post_ctrl[i]) / t_stat_p_ctrl)
            if (!is.na(se_ctrl) && se_ctrl >= 0 && !is.na(df$n_ctrl[i]) && df$n_ctrl[i] > 0) {
              sd_change_p_ctrl <- se_ctrl / sqrt(1 / df$n_ctrl[i])
            }
          }
        }
      }
    }

    if (derive_from %in% c("ci", "both")) {
      if (!any(is.na(c(df$upper_ci_int[i], df$lower_ci_int[i], df$n_int[i]))) && df$n_int[i] > 1) {
        sd_change_ci_int <- abs(((df$upper_ci_int[i] - df$lower_ci_int[i]) / 3.92) * sqrt(df$n_int[i] - 1))
      }
      if (!isTRUE(single_group)) {
        if (!any(is.na(c(df$upper_ci_ctrl[i], df$lower_ci_ctrl[i], df$n_ctrl[i]))) && df$n_ctrl[i] > 1) {
          sd_change_ci_ctrl <- abs(((df$upper_ci_ctrl[i] - df$lower_ci_ctrl[i]) / 3.92) * sqrt(df$n_ctrl[i] - 1))
        }
      }
    }

    if (is.na(sd_change_int[i])) {
      sd_change_int[i] <-
        if (derive_from == "p_value") sd_change_p_int else
          if (derive_from == "ci") sd_change_ci_int else
            ifelse(!is.na(sd_change_p_int), sd_change_p_int, sd_change_ci_int)
    }
    if (!isTRUE(single_group) && is.na(sd_change_ctrl[i])) {
      sd_change_ctrl[i] <-
        if (derive_from == "p_value") sd_change_p_ctrl else
          if (derive_from == "ci") sd_change_ci_ctrl else
            ifelse(!is.na(sd_change_p_ctrl), sd_change_p_ctrl, sd_change_ci_ctrl)
    }

    # mean_diff
    if (!any(is.na(c(df$mean_pre_int[i], df$mean_post_int[i])))) {
      mean_diff_int[i] <- df$mean_post_int[i] - df$mean_pre_int[i]
    }
    if (!isTRUE(single_group) && !any(is.na(c(df$mean_pre_ctrl[i], df$mean_post_ctrl[i])))) {
      mean_diff_ctrl[i] <- df$mean_post_ctrl[i] - df$mean_pre_ctrl[i]
    }

    # initial r (if data are sufficient)
    if (!any(is.na(c(df$sd_pre_int[i], df$sd_post_int[i], sd_change_int[i])))) {
      r_int[i] <- ((df$sd_pre_int[i]^2 + df$sd_post_int[i]^2 - sd_change_int[i]^2) /
                     (2 * df$sd_pre_int[i] * df$sd_post_int[i]))
    }
    if (!isTRUE(single_group) && !any(is.na(c(df$sd_pre_ctrl[i], df$sd_post_ctrl[i], sd_change_ctrl[i])))) {
      r_ctrl[i] <- ((df$sd_pre_ctrl[i]^2 + df$sd_post_ctrl[i]^2 - sd_change_ctrl[i]^2) /
                      (2 * df$sd_pre_ctrl[i] * df$sd_post_ctrl[i]))
    }
  }

  # -------------------------------------------------------------------
  # GLOBAL CV PRE-COMPUTATION (robust, no assumed r)
  # -------------------------------------------------------------------
  # Intervention
  cv_pool_int <- numeric(0)
  known_idx_int <- which(!is.na(sd_change_int) & !is.na(mean_diff_int) & mean_diff_int != 0)
  if (length(known_idx_int) > 0) {
    cv_pool_int <- c(cv_pool_int, sd_change_int[known_idx_int] / abs(mean_diff_int[known_idx_int]))
  }
  interval_idx_int <- which(is.na(sd_change_int) &
                              !is.na(df$sd_pre_int) & !is.na(df$sd_post_int) &
                              !is.na(mean_diff_int) & mean_diff_int != 0)
  if (length(interval_idx_int) > 0) {
    preI  <- df$sd_pre_int[interval_idx_int]
    postI <- df$sd_post_int[interval_idx_int]
    absdI <- abs(mean_diff_int[interval_idx_int])

    sd_minI <- sqrt(pmax(preI^2 + postI^2 - 2*0.9999*preI*postI, 0))
    sd_maxI <- sqrt(pmax(preI^2 + postI^2 - 2*(-0.9999)*preI*postI, 0))

    cv_minI <- sd_minI / absdI
    cv_maxI <- sd_maxI / absdI

    cv_repI <- sqrt(cv_minI * cv_maxI)
    cv_repI[!is.finite(cv_repI)] <- NA_real_
    cv_pool_int <- c(cv_pool_int, cv_repI)
  }
  cv_global_int <- suppressWarnings(stats::median(cv_pool_int, na.rm = TRUE))
  if (!is.finite(cv_global_int)) cv_global_int <- NA_real_

  # Control
  cv_global_ctrl <- NA_real_
  if (!isTRUE(single_group)) {
    cv_pool_ctrl <- numeric(0)
    known_idx_ctrl <- which(!is.na(sd_change_ctrl) & !is.na(mean_diff_ctrl) & mean_diff_ctrl != 0)
    if (length(known_idx_ctrl) > 0) {
      cv_pool_ctrl <- c(cv_pool_ctrl, sd_change_ctrl[known_idx_ctrl] / abs(mean_diff_ctrl[known_idx_ctrl]))
    }
    interval_idx_ctrl <- which(is.na(sd_change_ctrl) &
                                 !is.na(df$sd_pre_ctrl) & !is.na(df$sd_post_ctrl) &
                                 !is.na(mean_diff_ctrl) & mean_diff_ctrl != 0)
    if (length(interval_idx_ctrl) > 0) {
      preC  <- df$sd_pre_ctrl[interval_idx_ctrl]
      postC <- df$sd_post_ctrl[interval_idx_ctrl]
      absdC <- abs(mean_diff_ctrl[interval_idx_ctrl])

      sd_minC <- sqrt(pmax(preC^2 + postC^2 - 2*0.9999*preC*postC, 0))
      sd_maxC <- sqrt(pmax(preC^2 + postC^2 - 2*(-0.9999)*preC*postC, 0))

      cv_minC <- sd_minC / absdC
      cv_maxC <- sd_maxC / absdC

      cv_repC <- sqrt(cv_minC * cv_maxC)
      cv_repC[!is.finite(cv_repC)] <- NA_real_
      cv_pool_ctrl <- c(cv_pool_ctrl, cv_repC)
    }
    cv_global_ctrl <- suppressWarnings(stats::median(cv_pool_ctrl, na.rm = TRUE))
    if (!is.finite(cv_global_ctrl)) cv_global_ctrl <- NA_real_
  }

  # -------------------------------------------------------------------
  # BLOCK: maxima for the "direct" imputation method
  # -------------------------------------------------------------------
  sd_change_int_max  <- if (any(!is.na(sd_change_int)))  max(sd_change_int,  na.rm = TRUE) else NA_real_
  sd_change_ctrl_max <- if (any(!is.na(sd_change_ctrl))) max(sd_change_ctrl, na.rm = TRUE) else NA_real_

  if (is.na(sd_change_int_max) && impute_method == "direct") {
    warning("No real sd_change values available to impute (intervention).")
  }
  if (!isTRUE(single_group) && is.na(sd_change_ctrl_max) && impute_method == "direct") {
    warning("No real sd_change values available to impute (control).")
  }

  # -------------------------------------------------------------------
  # SECOND LOOP: NA imputation, recompute r, range warnings
  # -------------------------------------------------------------------
  for (i in seq_len(n)) {
    imputed_int  <- FALSE
    imputed_ctrl <- FALSE

    # --- Manual override (custom) — INTERVENTION ---
    if (!is.null(custom_sd_change_int)) {
      for (entry in custom_sd_change_int) {
        if (!all(c("row", "value") %in% names(entry))) next
        row_idx <- as.integer(entry$row)
        val <- as.numeric(entry$value)
        if (row_idx == i) {
          sd_change_int[i] <- val
          imputed_int <- TRUE
          if (verbose) message(sprintf("Custom sd_change_int set at row %d: %.4f", i, val))
          if (isTRUE(report_imputations)) {
            pre <- df$sd_pre_int[i]; post <- df$sd_post_int[i]
            if (!is.na(pre) && !is.na(post)) {
              sd_min <- sqrt(pre^2 + post^2 - 2 * 0.9999 * pre * post)
              sd_max <- sqrt(pre^2 + post^2 - 2 * (-0.9999) * pre * post)
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]],
                                       sprintf("sd_change_int (manual: %.4f, suggested_range: [%.4f, %.4f])", val, sd_min, sd_max)))
            } else {
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_change_int (manual: %.4f)", val)))
            }
          }
        }
      }
    }

    # --- Automated imputation — INTERVENTION (if still NA) ---
    if (is.na(sd_change_int[i])) {
      if (impute_method == "direct") {
        if (is.finite(sd_change_int_max)) {
          sd_change_int[i] <- sd_change_int_max
          imputed_int <- TRUE
          if (verbose) message(sprintf("Imputed sd_change_int at row %d using 'direct': %0.4f", i, sd_change_int[i]))
        }
      } else if (impute_method == "mean") {
        valid_mean <- mean(sd_change_int, na.rm = TRUE)
        if (is.finite(valid_mean)) {
          sd_change_int[i] <- valid_mean
          imputed_int <- TRUE
          if (verbose) message(sprintf("Imputed sd_change_int at row %d using 'mean': %0.4f", i, sd_change_int[i]))
        }
      } else if (impute_method == "cv") {
        if (is.finite(cv_global_int) && !is.na(mean_diff_int[i]) && mean_diff_int[i] != 0) {
          sd_change_int[i] <- abs(mean_diff_int[i]) * cv_global_int
          imputed_int <- TRUE
          if (verbose) message(sprintf("Imputed sd_change_int at row %d using 'cv' (global median): %.4f", i, sd_change_int[i]))
        }
      }

      if (isTRUE(report_imputations) && imputed_int) {
        if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
        imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_change_int (%s: %.4f)", impute_method, sd_change_int[i])))
      }
    }

    # --- Recompute r_int after imputation/custom ---
    if (!is.na(df$sd_pre_int[i]) && !is.na(df$sd_post_int[i]) && !is.na(sd_change_int[i])) {
      r_temp <- ((df$sd_pre_int[i]^2 + df$sd_post_int[i]^2 - sd_change_int[i]^2) /
                   (2 * df$sd_pre_int[i] * df$sd_post_int[i]))
      if (!is.na(r_temp) && (r_temp <= -0.9999 || r_temp >= 0.9999)) {
        sd_min <- sqrt(df$sd_pre_int[i]^2 + df$sd_post_int[i]^2 - 2 * 0.9999 * df$sd_pre_int[i] * df$sd_post_int[i])
        sd_max <- sqrt(df$sd_pre_int[i]^2 + df$sd_post_int[i]^2 - 2 * (-0.9999) * df$sd_pre_int[i] * df$sd_post_int[i])
        warning(sprintf(
          paste0("Row %d: Imputed sd_change_int = %.4f gives r_int = %.4f (outside [-0.9999, 0.9999]).\n",
                 "Suggested sd_change_int range: [%.4f, %.4f].\n r_int not assigned."),
          i, sd_change_int[i], r_temp, sd_min, sd_max
        ))
        r_int[i] <- NA_real_
      } else {
        r_int[i] <- r_temp
        if (isTRUE(report_imputations) && imputed_int && !is.na(r_int[i]) && r_int[i] > -0.9999 && r_int[i] < 0.9999) {
          if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
          imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("r_int (%s: %.4f)", impute_method, r_int[i])))
        }
      }
    }

    # --- Manual override (custom) — CONTROL ---
    if (!isTRUE(single_group) && !is.null(custom_sd_change_ctrl)) {
      for (entry in custom_sd_change_ctrl) {
        if (!all(c("row", "value") %in% names(entry))) next
        row_idx <- as.integer(entry$row)
        val <- as.numeric(entry$value)
        if (row_idx == i) {
          sd_change_ctrl[i] <- val
          imputed_ctrl <- TRUE
          if (verbose) message(sprintf("Custom sd_change_ctrl set at row %d: %.4f", i, val))
          if (isTRUE(report_imputations)) {
            pre <- df$sd_pre_ctrl[i]; post <- df$sd_post_ctrl[i]
            if (!is.na(pre) && !is.na(post)) {
              sd_min <- sqrt(pre^2 + post^2 - 2 * 0.9999 * pre * post)
              sd_max <- sqrt(pre^2 + post^2 - 2 * (-0.9999) * pre * post)
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]],
                                       sprintf("sd_change_ctrl (manual: %.4f, suggested_range: [%.4f, %.4f])", val, sd_min, sd_max)))
            } else {
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_change_ctrl (manual: %.4f)", val)))
            }
          }
        }
      }
    }

    # --- Automated imputation — CONTROL (if still NA) ---
    if (!isTRUE(single_group) && is.na(sd_change_ctrl[i])) {
      if (impute_method == "direct") {
        if (is.finite(sd_change_ctrl_max)) {
          sd_change_ctrl[i] <- sd_change_ctrl_max
          imputed_ctrl <- TRUE
          if (verbose) message(sprintf("Imputed sd_change_ctrl at row %d using 'direct': %0.4f", i, sd_change_ctrl[i]))
        }
      } else if (impute_method == "mean") {
        valid_mean <- mean(sd_change_ctrl, na.rm = TRUE)
        if (is.finite(valid_mean)) {
          sd_change_ctrl[i] <- valid_mean
          imputed_ctrl <- TRUE
          if (verbose) message(sprintf("Imputed sd_change_ctrl at row %d using 'mean': %0.4f", i, sd_change_ctrl[i]))
        }
      } else if (impute_method == "cv") {
        if (is.finite(cv_global_ctrl) && !is.na(mean_diff_ctrl[i]) && mean_diff_ctrl[i] != 0) {
          sd_change_ctrl[i] <- abs(mean_diff_ctrl[i]) * cv_global_ctrl
          imputed_ctrl <- TRUE
          if (verbose) message(sprintf("Imputed sd_change_ctrl at row %d using 'cv' (global median): %.4f", i, sd_change_ctrl[i]))
        }
      }

      if (isTRUE(report_imputations) && imputed_ctrl) {
        if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
        imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_change_ctrl (%s: %.4f)", impute_method, sd_change_ctrl[i])))
      }
    }

    # --- Recompute r_ctrl after imputation/custom ---
    if (!isTRUE(single_group) && !any(is.na(c(df$sd_pre_ctrl[i], df$sd_post_ctrl[i], sd_change_ctrl[i])))) {
      r_temp <- ((df$sd_pre_ctrl[i]^2 + df$sd_post_ctrl[i]^2 - sd_change_ctrl[i]^2) /
                   (2 * df$sd_pre_ctrl[i] * df$sd_post_ctrl[i]))
      if (!is.na(r_temp) && (r_temp <= -0.9999 || r_temp >= 0.9999)) {
        sd_min <- sqrt(df$sd_pre_ctrl[i]^2 + df$sd_post_ctrl[i]^2 - 2 * 0.9999 * df$sd_pre_ctrl[i] * df$sd_post_ctrl[i])
        sd_max <- sqrt(df$sd_pre_ctrl[i]^2 + df$sd_post_ctrl[i]^2 - 2 * (-0.9999) * df$sd_pre_ctrl[i] * df$sd_post_ctrl[i])

        warning(sprintf(
          paste0("Row %d: Imputed sd_change_ctrl = %.4f gives r_ctrl = %.4f (outside [-0.9999, 0.9999]).\n",
                 "Suggested sd_change_ctrl range: [%.4f, %.4f].\n r_ctrl not assigned."),
          i, sd_change_ctrl[i], r_temp, sd_min, sd_max
        ))
        r_ctrl[i] <- NA_real_
      } else {
        r_ctrl[i] <- r_temp
        if (isTRUE(report_imputations) && imputed_ctrl && !is.na(r_ctrl[i]) && r_ctrl[i] > -0.9999 && r_ctrl[i] < 0.9999) {
          if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
          imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("r_ctrl (%s: %.4f)", impute_method, r_ctrl[i])))
        }
      }
    }
  }

  # -------------------------------------------------------------------
  # THIRD LOOP: Hedges correction and effect sizes
  # -------------------------------------------------------------------
  for (i in seq_len(n)) {

    # Hedges correction factor per group
    J_int  <- if (apply_hedges && !is.na(df$n_int[i])  && df$n_int[i]  > 1) 1 - (3 / (4 * (df$n_int[i]  - 1) - 1)) else 1
    J_ctrl <- if (!isTRUE(single_group) && apply_hedges && !is.na(df$n_ctrl[i]) && df$n_ctrl[i] > 1) 1 - (3 / (4 * (df$n_ctrl[i] - 1) - 1)) else 1

    # --- SMDs ---
    if (effect_size == "smd_pre") {
      if (!is.na(df$sd_pre_int[i]) && df$sd_pre_int[i] != 0) {
        smd_pre_int[i] <- J_int * (mean_diff_int[i] / df$sd_pre_int[i])
        var_smd_pre_int[i] <- (J_int^2) * ((2 * (1 - r_int[i])) / df$n_int[i]) *
          ((df$n_int[i] - 1)/(df$n_int[i] - 3)) *
          (1 + (df$n_int[i] * smd_pre_int[i]^2)/(2 * (1 - r_int[i]))) - smd_pre_int[i]^2
        se_smd_pre_int[i] <- sqrt(var_smd_pre_int[i])
      }

      if (!isTRUE(single_group) && !is.na(df$sd_pre_ctrl[i]) && df$sd_pre_ctrl[i] != 0) {
        smd_pre_ctrl[i] <- J_ctrl * (mean_diff_ctrl[i] / df$sd_pre_ctrl[i])
        var_smd_pre_ctrl[i] <- (J_ctrl^2) * ((2 * (1 - r_ctrl[i])) / df$n_ctrl[i]) *
          ((df$n_ctrl[i] - 1)/(df$n_ctrl[i] - 3)) *
          (1 + (df$n_ctrl[i] * smd_pre_ctrl[i]^2)/(2 * (1 - r_ctrl[i]))) - smd_pre_ctrl[i]^2
        se_smd_pre_ctrl[i] <- sqrt(var_smd_pre_ctrl[i])
      }

    } else if (effect_size == "smd_change") {
      if (!is.na(sd_change_int[i]) && sd_change_int[i] != 0) {
        smd_change_int[i] <- J_int * (mean_diff_int[i] / sd_change_int[i])
        var_smd_change_int[i] <- (J_int^2) * (1 / df$n_int[i]) * ((df$n_int[i] - 1)/(df$n_int[i] - 3)) *
          (1 + df$n_int[i] * smd_change_int[i]^2) - smd_change_int[i]^2
        se_smd_change_int[i] <- sqrt(var_smd_change_int[i])
      }
      if (!isTRUE(single_group) && !is.na(sd_change_ctrl[i]) && sd_change_ctrl[i] != 0) {
        smd_change_ctrl[i] <- J_ctrl * (mean_diff_ctrl[i] / sd_change_ctrl[i])
        var_smd_change_ctrl[i] <- (J_ctrl^2) * (1 / df$n_ctrl[i]) * ((df$n_ctrl[i] - 1)/(df$n_ctrl[i] - 3)) *
          (1 + df$n_ctrl[i] * smd_change_ctrl[i]^2) - smd_change_ctrl[i]^2
        se_smd_change_ctrl[i] <- sqrt(var_smd_change_ctrl[i])
      }

    } else if (effect_size == "smd_pooled") {
      if (!isTRUE(single_group) &&
          !any(is.na(c(df$n_int[i], df$n_ctrl[i], df$sd_pre_int[i], df$sd_pre_ctrl[i],
                       df$mean_pre_int[i], df$mean_post_int[i], df$mean_pre_ctrl[i], df$mean_post_ctrl[i],
                       r_int[i], r_ctrl[i])))) {
        pooled_sd <- sqrt(((df$n_int[i] - 1) * df$sd_pre_int[i]^2 + (df$n_ctrl[i] - 1) * df$sd_pre_ctrl[i]^2) /
                            (df$n_int[i] + df$n_ctrl[i] - 2))
        c_factor <- if (apply_hedges) 1 - (3 / (4 * (df$n_int[i] + df$n_ctrl[i] - 2) - 1)) else 1
        smd_pooled[i] <- c_factor * ((df$mean_pre_int[i] - df$mean_post_int[i]) -
                                       (df$mean_pre_ctrl[i] - df$mean_post_ctrl[i])) / pooled_sd

        r_m <- (r_int[i] + r_ctrl[i]) / 2
        n1 <- df$n_int[i]; n2 <- df$n_ctrl[i]
        var_smd_pooled[i] <- (c_factor^2) * 2 * (1 - r_m) * ((n1 + n2)/(n1 * n2)) *
          ((n1 + n2 - 2)/(n1 + n2 - 4)) *
          (1 + ((n1 * n2 * smd_pooled[i]^2)/(2 * (1 - r_m) * (n1 + n2)))) - smd_pooled[i]^2
        se_smd_pooled[i] <- sqrt(var_smd_pooled[i])
      }

    } else if (effect_size == "smd_diff_groups") {
      # Per-group simple SMDs
      if (!any(is.na(c(df$mean_pre_int[i], df$mean_post_int[i], df$sd_pre_int[i]))) && df$sd_pre_int[i] != 0) {
        smd_simple_int <- (df$mean_pre_int[i] - df$mean_post_int[i]) / df$sd_pre_int[i]
      } else {
        smd_simple_int <- NA_real_
      }
      if (!isTRUE(single_group) && !any(is.na(c(df$mean_pre_ctrl[i], df$mean_post_ctrl[i], df$sd_pre_ctrl[i]))) && df$sd_pre_ctrl[i] != 0) {
        smd_simple_ctrl <- (df$mean_pre_ctrl[i] - df$mean_post_ctrl[i]) / df$sd_pre_ctrl[i]
      } else {
        smd_simple_ctrl <- NA_real_
      }

      smd_diff_groups[i] <- if (!isTRUE(single_group)) smd_simple_int - smd_simple_ctrl else NA_real_

      # Per-group corrected variances
      if (!is.na(smd_simple_int) && !is.na(r_int[i]) && !is.na(df$n_int[i]) && df$n_int[i] > 3) {
        var_smd_simple_int <- (J_int^2) *
          ((2 * (1 - r_int[i])) / df$n_int[i]) *
          ((df$n_int[i] - 1) / (df$n_int[i] - 3)) *
          (1 + (df$n_int[i] * smd_simple_int^2) / (2 * (1 - r_int[i]))) -
          smd_simple_int^2
      } else var_smd_simple_int <- NA_real_

      if (!isTRUE(single_group) && !is.na(smd_simple_ctrl) && !is.na(r_ctrl[i]) && !is.na(df$n_ctrl[i]) && df$n_ctrl[i] > 3) {
        var_smd_simple_ctrl <- (J_ctrl^2) *
          ((2 * (1 - r_ctrl[i])) / df$n_ctrl[i]) *
          ((df$n_ctrl[i] - 1) / (df$n_ctrl[i] - 3)) *
          (1 + (df$n_ctrl[i] * smd_simple_ctrl^2) / (2 * (1 - r_ctrl[i]))) -
          smd_simple_ctrl^2
      } else var_smd_simple_ctrl <- NA_real_

      var_smd_diff_groups[i] <- if (!isTRUE(single_group)) var_smd_simple_int + var_smd_simple_ctrl else NA_real_
      se_smd_diff_groups[i]  <- if (!is.na(var_smd_diff_groups[i])) sqrt(var_smd_diff_groups[i]) else NA_real_
    }

    # % change
    if (!any(is.na(c(mean_diff_int[i], df$mean_pre_int[i]))) && df$mean_pre_int[i] != 0) {
      pct_change_int[i] <- (mean_diff_int[i] / df$mean_pre_int[i]) * 100
    }
    if (!isTRUE(single_group) && !any(is.na(c(mean_diff_ctrl[i], df$mean_pre_ctrl[i]))) && df$mean_pre_ctrl[i] != 0) {
      pct_change_ctrl[i] <- (mean_diff_ctrl[i] / df$mean_pre_ctrl[i]) * 100
    }

    # Mean differences (optional)
    if (mean_differences) {
      if (!any(is.na(c(sd_change_int[i], df$n_int[i]))) && df$n_int[i] > 0) {
        var_mean_diff_int[i] <- sd_change_int[i]^2 / df$n_int[i]
        se_mean_diff_int[i]  <- sqrt(var_mean_diff_int[i])
      }
      if (!isTRUE(single_group) && !any(is.na(c(sd_change_ctrl[i], df$n_ctrl[i]))) && df$n_ctrl[i] > 0) {
        var_mean_diff_ctrl[i] <- sd_change_ctrl[i]^2 / df$n_ctrl[i]
        se_mean_diff_ctrl[i]  <- sqrt(var_mean_diff_ctrl[i])
      }
    }

    # Per-row rounding (if requested)
    if (!is.null(digits)) {
      vars_to_round <- c("r_int", "r_ctrl",
                         "smd_pre_int", "smd_pre_ctrl", "var_smd_pre_int", "var_smd_pre_ctrl", "se_smd_pre_int", "se_smd_pre_ctrl",
                         "smd_change_int", "smd_change_ctrl", "var_smd_change_int", "var_smd_change_ctrl", "se_smd_change_int", "se_smd_change_ctrl",
                         "smd_pooled", "var_smd_pooled", "se_smd_pooled",
                         "mean_diff_int", "mean_diff_ctrl", "var_mean_diff_int", "var_mean_diff_ctrl", "se_mean_diff_int", "se_mean_diff_ctrl",
                         "pct_change_int", "pct_change_ctrl",
                         "smd_diff_groups", "var_smd_diff_groups", "se_smd_diff_groups",
                         "sd_change_int", "sd_change_ctrl")
      for (varname in vars_to_round) {
        if (exists(varname, inherits = FALSE)) {
          val <- get(varname, inherits = FALSE)[i]
          if (!is.na(val)) {
            vec <- get(varname, inherits = FALSE)
            vec[i] <- round(val, digits)
            assign(varname, vec)
          }
        }
      }
    }
  }

  # -------------------------------------------------------------------
  # BLOCK: Word imputation report
  # -------------------------------------------------------------------
  if (isTRUE(report_imputations)) {
    style_normal <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    style_bold   <- officer::fp_text(font.size = 12, font.family = "Times New Roman", bold = TRUE)
    style_sub    <- officer::fp_text(font.size = 12, font.family = "Times New Roman", vertical.align = "subscript")

    doc <- officer::read_docx()
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Imputation Report", style_bold)))
    doc <- officer::body_add_par(doc, "", style = "Normal")

    n_sd_int  <- sum(grepl("^sd_change_int",  unlist(imp_log)))
    n_sd_ctrl <- sum(grepl("^sd_change_ctrl", unlist(imp_log)))
    n_r_int   <- sum(grepl("^r_int",  unlist(imp_log)))
    n_r_ctrl  <- sum(grepl("^r_ctrl", unlist(imp_log)))
    n_imputaciones <- n_sd_int + n_sd_ctrl + n_r_int + n_r_ctrl

    if (isTRUE(single_group)) {
      doc <- officer::body_add_fpar(doc, officer::fpar(
        officer::ftext(sprintf("A total of %d imputations were performed: ", n_imputaciones), style_normal),
        officer::ftext(sprintf("%d for SD", n_sd_int), style_normal),
        officer::ftext("change", style_sub),
        officer::ftext(" (intervention), ", style_normal),
        officer::ftext(sprintf("%d for r (intervention).", n_r_int), style_normal)
      ))
    } else {
      doc <- officer::body_add_fpar(doc, officer::fpar(
        officer::ftext(sprintf("A total of %d imputations were performed: %d for SD", n_imputaciones, n_sd_int), style_normal),
        officer::ftext("change", style_sub),
        officer::ftext(" (intervention), ", style_normal),
        officer::ftext(sprintf("%d for SD", n_sd_ctrl), style_normal),
        officer::ftext("change", style_sub),
        officer::ftext(" (control), ", style_normal),
        officer::ftext(sprintf("%d for r (intervention), %d for r (control).", n_r_int, n_r_ctrl), style_normal)
      ))
    }

    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("Missing values were identified for the standard deviation of the change scores (", style_normal),
      officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
      officer::ftext(") and/or the Pearson correlation coefficient (r). Imputations were carried out using the specified method (i.e., mean, direct, cv, or manual entry) only when sufficient descriptive data were available (Fu et al., 2013).", style_normal)
    ))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Studies requiring imputation", style_bold)))
    doc <- officer::body_add_par(doc, "", style = "Normal")

    for (study in unique(df$study_name)) {
      rows <- which(df$study_name == study)
      imp_vars <- unlist(imp_log[rows])
      if (length(imp_vars) > 0) {
        doc <- officer::body_add_par(doc, paste0("- ", study, ":"), style = "Normal")
        for (i in rows) {
          # Intervention group - sd_change imputations
          logs_sd_int <- imp_log[[i]][grepl("^sd_change_int", imp_log[[i]])]
          for (log in logs_sd_int) {
            matches_manual_range <- stringr::str_match(
              log, "sd_change_int \\(manual: ([0-9.]+), suggested_range: \\[([0-9.eE+-]+), ([0-9.eE+-]+)\\]\\)"
            )
            if (!any(is.na(matches_manual_range))) {
              valor <- as.numeric(matches_manual_range[2])
              rango_min <- as.numeric(matches_manual_range[3])
              rango_max <- as.numeric(matches_manual_range[4])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(" for the intervention group was manually entered as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(" (suggested ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(paste0(" range: [", sprintf("%.2f", rango_min), ", ", sprintf("%.2f", rango_max), "])."), style_normal)
              ))
              next
            }
            matches_manual <- stringr::str_match(log, "sd_change_int \\(manual: ([0-9.]+)\\)")
            if (!any(is.na(matches_manual))) {
              valor <- as.numeric(matches_manual[2])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(paste0(" for the intervention group was manually entered as ", sprintf("%.2f", valor), "."), style_normal)
              ))
              next
            }
            matches <- stringr::str_match(log, "sd_change_int \\((\\w+): ([0-9.\\-]+)\\)")
            if (!any(is.na(matches))) {
              metodo <- matches[2]; valor <- as.numeric(matches[3])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The SD", style_normal),
                officer::ftext("change", style_sub),
                officer::ftext(" for the intervention group was imputed as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(sprintf(" using the %s method.", metodo), style_normal)
              ))
            }
          }
          # Intervention group - r_int imputations
          logs_r_int <- imp_log[[i]][grepl("^r_int", imp_log[[i]])]
          for (log in logs_r_int) {
            matches_manual <- stringr::str_match(log, "r_int \\(manual: ([0-9.\\-]+)\\)")
            if (!any(is.na(matches_manual))) {
              valor <- as.numeric(matches_manual[2])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The r for the intervention group was manually entered as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(".", style_normal)
              ))
              next
            }
            matches <- stringr::str_match(log, "r_int \\((\\w+): ([0-9.\\-]+)\\)")
            if (!any(is.na(matches))) {
              metodo <- matches[2]; valor <- as.numeric(matches[3])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The r for the intervention group was estimated as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(sprintf(", using the %s method, based on the imputed ", metodo), style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(".", style_normal)
              ))
            }
          }
          # Control group - sd_change imputations
          logs_sd_ctrl <- imp_log[[i]][grepl("^sd_change_ctrl", imp_log[[i]])]
          for (log in logs_sd_ctrl) {
            matches_manual_range <- stringr::str_match(
              log, "sd_change_ctrl \\(manual: ([0-9.]+), suggested_range: \\[([0-9.eE+-]+), ([0-9.eE+-]+)\\]\\)"
            )
            if (!any(is.na(matches_manual_range))) {
              valor <- as.numeric(matches_manual_range[2])
              rango_min <- as.numeric(matches_manual_range[3])
              rango_max <- as.numeric(matches_manual_range[4])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(" for the control group was manually entered as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(" (suggested ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(paste0(" range: [", sprintf("%.2f", rango_min), ", ", sprintf("%.2f", rango_max), "])."), style_normal)
              ))
              next
            }
            matches_manual <- stringr::str_match(log, "sd_change_ctrl \\(manual: ([0-9.]+)\\)")
            if (!any(is.na(matches_manual))) {
              valor <- as.numeric(matches_manual[2])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(paste0(" for the control group was manually entered as ", sprintf("%.2f", valor), "."), style_normal)
              ))
              next
            }
            matches <- stringr::str_match(log, "sd_change_ctrl \\((\\w+): ([0-9.\\-]+)\\)")
            if (!any(is.na(matches))) {
              metodo <- matches[2]; valor <- as.numeric(matches[3])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The SD", style_normal),
                officer::ftext("change", style_sub),
                officer::ftext(" for the control group was imputed as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(sprintf(" using the %s method.", metodo), style_normal)
              ))
            }
          }
          # Control group - r_ctrl imputations
          logs_r_ctrl <- imp_log[[i]][grepl("^r_ctrl", imp_log[[i]])]
          for (log in logs_r_ctrl) {
            matches_manual <- stringr::str_match(log, "r_ctrl \\(manual: ([0-9.\\-]+)\\)")
            if (!any(is.na(matches_manual))) {
              valor <- as.numeric(matches_manual[2])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The r for the control group was manually entered as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(".", style_normal)
              ))
              next
            }
            matches <- stringr::str_match(log, "r_ctrl \\((\\w+): ([0-9.\\-]+)\\)")
            if (!any(is.na(matches))) {
              metodo <- matches[2]; valor <- as.numeric(matches[3])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The r for the control group was estimated as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(sprintf(", using the %s method, based on the imputed ", metodo), style_normal),
                officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
                officer::ftext(".", style_normal)
              ))
            }
          }
        }
        doc <- officer::body_add_par(doc, "", style = "Normal")
      }
    }
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Imputation Rationale", style_bold)))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("When ", style_normal),
      officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
      officer::ftext(" values were missing, they were estimated based on available data from other studies included in the meta-analysis.", style_normal)
    ))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("The mean method substituted missing values with the average ", style_normal),
      officer::ftext("SD", style_normal), officer::ftext("change", style_sub),
      officer::ftext(" across valid cases. The cv method used |mean difference| multiplied by a robust global median of cv obtained from cases with known SD", style_normal),
      officer::ftext("change", style_sub),
      officer::ftext(" or feasible intervals. The direct method used the maximum available SD", style_normal),
      officer::ftext("change", style_sub),
      officer::ftext(" from previous valid studies.", style_normal)
    ))
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("Manually entered values were supplied by the user and are documented above. This report ensures transparency by detailing all imputations performed, supporting reproducibility of the meta-analytic process.", style_normal)
    ))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("These strategies are commonly employed in meta-analytic practice when summary statistics are incomplete and are consistent with guidance for handling missing variance data in systematic reviews and meta-analyses", style_normal),
      officer::ftext(" (Higgins et al., 2023).", style_normal)
    ))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("References", style_bold)))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("Higgins, J. P. T., Thomas, J., Chandler, J., Cumpston, M., Li, T., Page, M. J., & Welch, V. A. (Eds.). (2023). ", style_normal),
      officer::ftext("Cochrane handbook for systematic reviews of interventions ", style_normal),
      officer::ftext("(Version 6.3). Cochrane. ", style_normal),
      officer::ftext("https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current", style_normal)
    ))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("Fu, R., Vandermeer, B.W., Shamliyan, T.A., ONeil, M.E., Yazdi, F., Fox, S.H., & Morton, S.C. (2013). ", style_normal),
      officer::ftext("Handling Continuous Outcomes in Quantitative Synthesis. Methods Guide for Comparative Effectiveness Reviews. ", style_normal),
      officer::ftext("(Prepared by the Oregon Evidence-based Practice Center under Contract No. 290-2007-10057-I.) ", style_normal),
      officer::ftext("AHRQ Publication No. 13-EHC103-EF. Rockville, MD: Agency for Healthcare Research and Quality. July 2013. ", style_normal),
      officer::ftext("https://effectivehealthcare.ahrq.gov/reports/final.cfm", style_normal)
    ))
    print(doc, target = "imputation_report.docx")
  }

  # -------------------------------------------------------------------
  # BLOCK: build the output data frame
  # -------------------------------------------------------------------
  if (add_to_df) {
    df$r_int          <- r_int
    df$sd_change_int  <- sd_change_int
    df$pct_change_int <- pct_change_int

    if (!isTRUE(single_group)) {
      df$r_ctrl          <- r_ctrl
      df$sd_change_ctrl  <- sd_change_ctrl
      df$pct_change_ctrl <- pct_change_ctrl
    }

    if (effect_size == "smd_pre") {
      df$smd_pre_int     <- smd_pre_int
      df$var_smd_pre_int <- var_smd_pre_int
      df$se_smd_pre_int  <- se_smd_pre_int
      if (!isTRUE(single_group)) {
        df$smd_pre_ctrl     <- smd_pre_ctrl
        df$var_smd_pre_ctrl <- var_smd_pre_ctrl
        df$se_smd_pre_ctrl  <- se_smd_pre_ctrl
      }
    }

    if (effect_size == "smd_change") {
      df$smd_change_int     <- smd_change_int
      df$var_smd_change_int <- var_smd_change_int
      df$se_smd_change_int  <- se_smd_change_int
      if (!isTRUE(single_group)) {
        df$smd_change_ctrl     <- smd_change_ctrl
        df$var_smd_change_ctrl <- var_smd_change_ctrl
        df$se_smd_change_ctrl  <- se_smd_change_ctrl
      }
    }

    if (effect_size == "smd_pooled" && !isTRUE(single_group)) {
      df$smd_pooled     <- smd_pooled
      df$var_smd_pooled <- var_smd_pooled
      df$se_smd_pooled  <- se_smd_pooled
    }

    if (effect_size == "smd_diff_groups") {
      df$smd_diff_groups     <- smd_diff_groups
      df$var_smd_diff_groups <- var_smd_diff_groups
      df$se_smd_diff_groups  <- se_smd_diff_groups
    }

    if (mean_differences) {
      df$mean_diff_int     <- mean_diff_int
      df$var_mean_diff_int <- var_mean_diff_int
      df$se_mean_diff_int  <- se_mean_diff_int
      if (!isTRUE(single_group)) {
        df$mean_diff_ctrl     <- mean_diff_ctrl
        df$var_mean_diff_ctrl <- var_mean_diff_ctrl
        df$se_mean_diff_ctrl  <- se_mean_diff_ctrl
      }
    }

    # Drop dummy NA control columns added at the top (when no real control data)
    dummy_cols <- c("mean_pre_ctrl", "mean_post_ctrl",
                    "sd_pre_ctrl", "sd_post_ctrl",
                    "upper_ci_ctrl", "lower_ci_ctrl",
                    "n_ctrl", "p_value_ctrl")
    for (col in dummy_cols) {
      if (all(is.na(df[[col]]))) {
        df[[col]] <- NULL
      }
    }

    return(df)
  }
}
