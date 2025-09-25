#' Effect Sizes and Imputation for Meta-Analysis of Pre-Post Studies and Pre-Post intervention and control groups studies (metacor_dual)
#'
#' Calculates effect sizes (i.e., SMDpre, SMDchange, ScMDpooled, ScMDpre) and allows for various imputation methods (i.e., none, cv, direct, mean)
#' for missing SDdiff and correlation coefficients in pre-post meta-analyses, with or without a control group.
#' Generates a detailed imputation report in Word format.
#'
#' @param df Data frame with the necessary columns for intervention and (optionally) control groups.
#' @param digits Number of decimal places to round results (default: NULL).
#' @param add_to_df Logical. If TRUE, results are added to the original data frame.
#' @param method Method for SDdiff calculation (i.e., 'p_value', 'ci', 'both').
#' @param apply_hedges Logical. Apply Hedges' g correction? (default: TRUE)
#' @param SMD_method Method for effect size (i.e., 'SMDpre', 'SMDchange', 'ScMDpooled', 'ScMDpre').
#' @param MeanDifferences Logical. Calculate mean differences and variances? (default: FALSE)
#' @param impute_method Imputation method for missing SDdiff (i.e., 'none', 'direct', 'mean', 'cv').
#' @param verbose Logical. Print messages during processing? (default: TRUE)
#' @param report_imputations Logical. Generate Word imputation report? (default: FALSE)
#' @param custom_sd_diff_int List with elements 'row' and 'value' for manual sd_diff_int values.
#' @param custom_sd_diff_con List with elements 'row' and 'value' for manual sd_diff_con values.
#' @param single_group Logical. Is the design single-group only? (default: FALSE)
#' @return Data frame with calculated variables. Optionally, a Word report ('imputation_report.docx') is generated.
#' @export
#' @importFrom stats qt
#' @importFrom stringr str_match str_detect
#' @importFrom officer read_docx body_add_fpar body_add_par fpar ftext fp_text
#' @examples
#' df <- data.frame(
#'   study_name = c("Study1", "Study2", "Study3", "Study4",
#'   "Study5", "Study6", "Study7", "Study8", "Study9"),
#'   p_value_Int = c(1.038814e-07, NA, NA, NA, NA, 2.100000e-02, NA, NA, NA),
#'   n_Int = c(10, 10, 10, 10, 15, 15, 10, 10, 10),
#'   meanPre_Int = c(8.17, 10.09, 10.18, 9.85, 9.51, 7.70, 10.00, 11.53, 11.20),
#'   meanPost_Int = c(10.12, 12.50, 12.56, 10.41, 10.88, 9.20, 10.80, 13.42, 12.00),
#'   sd_pre_Int = c(1.83, 0.67, 0.66, 0.90, 0.62, 0.90, 0.70, 0.60, 1.90),
#'   sd_post_Int = c(1.85, 0.72, 0.97, 0.67, 0.76, 1.10, 0.70, 0.80, 1.80),
#'   upperCI_Int = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
#'   lowerCI_Int = c(NA, NA, NA, NA, NA, NA, NA, NA, NA)
#' )
#' result <- metacor_dual(df)
#' print(result)
#' @references
#' Higgins, J. P. T., Thomas, J., Chandler, J., Cumpston, M., Li, T., Page, M. J., & Welch, V. A. (Eds.). (2023). Cochrane handbook for systematic reviews of interventions (Version 6.3). Cochrane. https://training.cochrane.org/handbook
#' Fu, R., Vandermeer, B.W., Shamliyan, T.A., ONeil, M.E., Yazdi, F., Fox, S.H., & Morton, S.C. (2013). Handling Continuous Outcomes in Quantitative Synthesis. Methods Guide for Comparative Effectiveness Reviews. AHRQ Publication No. 13-EHC103-EF. https://effectivehealthcare.ahrq.gov/reports/final.cfm
metacor_dual <- function(df, digits = NULL, add_to_df = TRUE, method = "both",
                         apply_hedges = TRUE, SMD_method = "SMDpre", MeanDifferences = FALSE,
                         impute_method = "none", verbose = TRUE, report_imputations = FALSE,
                         custom_sd_diff_int = NULL, custom_sd_diff_con = NULL, single_group = FALSE) {

  # Paquetes requeridos para el informe (solo si se usa)
  if (isTRUE(report_imputations)) {
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Package 'officer' is required for reporting. Please install it.")
    }
    if (!requireNamespace("stringr", quietly = TRUE)) {
      stop("Package 'stringr' is required for reporting. Please install it.")
    }
  }

  imp_log <- vector("list", nrow(df))

  # -------------------------
  # BLOQUE: Creación de columnas dummy para el grupo control si no existen
  # -------------------------
  control_vars <- c("meanPre_Con", "meanPost_Con", "sd_pre_Con", "sd_post_Con",
                    "upperCI_Con", "lowerCI_Con", "n_Con", "p_value_Con")

  for (var in control_vars) {
    if (!var %in% names(df)) {
      df[[var]] <- NA_real_
    }
  }

  # -------------------------
  # BLOQUE: Inicialización de vectores
  # -------------------------
  n <- nrow(df)
  r_int <- rep(NA_real_, n)
  r_con <- rep(NA_real_, n)

  SMDpre_int <- SMDpre_con <- varSMDpre_int <- varSMDpre_con <- seSMDpre_int <- seSMDpre_con <- rep(NA_real_, n)
  SMDchange_int <- SMDchange_con <- varSMDchange_int <- varSMDchange_con <- seSMDchange_int <- seSMDchange_con <- rep(NA_real_, n)
  ScMDpooled <- varScMDpooled <- seScMDpooled <- rep(NA_real_, n)
  ScMDpre <- var_ScMDpre <- se_ScMDpre <- rep(NA_real_, n)

  pct_change_int <- pct_change_con <- rep(NA_real_, n)
  sd_diff_int <- rep(NA_real_, n)
  sd_diff_con <- rep(NA_real_, n)
  meanDiff_int <- meanDiff_con <- varMeanDiff_int <- varMeanDiff_con <- seMeanDiff_int <- seMeanDiff_con <- rep(NA_real_, n)

  # -------------------------
  # PRIMER BUCLE: Calcular sd_diff inicial (p_value / CI), meanDiff y r (si procede)
  # -------------------------
  for (i in seq_len(n)) {

    sd_diff_p_int <- sd_diff_ci_int <- NA_real_
    sd_diff_p_con <- sd_diff_ci_con <- NA_real_

    if (method %in% c("p_value", "both")) {
      if (!is.na(df$p_value_Int[i]) && !is.na(df$n_Int[i]) && df$n_Int[i] > 1 &&
          !is.na(df$meanPre_Int[i]) && !is.na(df$meanPost_Int[i])) {
        t_stat_p_int <- stats::qt(df$p_value_Int[i] / 2, df$n_Int[i] - 1, lower.tail = FALSE)
        if (is.finite(t_stat_p_int) && t_stat_p_int != 0) {
          se_int <- abs((df$meanPre_Int[i] - df$meanPost_Int[i]) / t_stat_p_int)
          if (!is.na(se_int) && se_int >= 0 && !is.na(df$n_Int[i]) && df$n_Int[i] > 0) {
            sd_diff_p_int <- se_int / sqrt(1 / df$n_Int[i])
          }
        }
      }

      if (!isTRUE(single_group)) {
        if (!is.na(df$p_value_Con[i]) && !is.na(df$n_Con[i]) && df$n_Con[i] > 1 &&
            !is.na(df$meanPre_Con[i]) && !is.na(df$meanPost_Con[i])) {
          t_stat_p_con <- stats::qt(df$p_value_Con[i] / 2, df$n_Con[i] - 1, lower.tail = FALSE)
          if (is.finite(t_stat_p_con) && t_stat_p_con != 0) {
            se_con <- abs((df$meanPre_Con[i] - df$meanPost_Con[i]) / t_stat_p_con)
            if (!is.na(se_con) && se_con >= 0 && !is.na(df$n_Con[i]) && df$n_Con[i] > 0) {
              sd_diff_p_con <- se_con / sqrt(1 / df$n_Con[i])
            }
          }
        }
      }
    }

    if (method %in% c("ci", "both")) {
      if (!any(is.na(c(df$upperCI_Int[i], df$lowerCI_Int[i], df$n_Int[i]))) && df$n_Int[i] > 1) {
        sd_diff_ci_int <- abs(((df$upperCI_Int[i] - df$lowerCI_Int[i]) / 3.92) * sqrt(df$n_Int[i] - 1))
      }
      if (!isTRUE(single_group)) {
        if (!any(is.na(c(df$upperCI_Con[i], df$lowerCI_Con[i], df$n_Con[i]))) && df$n_Con[i] > 1) {
          sd_diff_ci_con <- abs(((df$upperCI_Con[i] - df$lowerCI_Con[i]) / 3.92) * sqrt(df$n_Con[i] - 1))
        }
      }
    }

    if (is.na(sd_diff_int[i])) {
      sd_diff_int[i] <-
        if (method == "p_value") sd_diff_p_int else
          if (method == "ci") sd_diff_ci_int else
            ifelse(!is.na(sd_diff_p_int), sd_diff_p_int, sd_diff_ci_int)
    }
    if (!isTRUE(single_group) && is.na(sd_diff_con[i])) {
      sd_diff_con[i] <-
        if (method == "p_value") sd_diff_p_con else
          if (method == "ci") sd_diff_ci_con else
            ifelse(!is.na(sd_diff_p_con), sd_diff_p_con, sd_diff_ci_con)
    }

    # meanDiff
    if (!any(is.na(c(df$meanPre_Int[i], df$meanPost_Int[i])))) {
      meanDiff_int[i] <- df$meanPost_Int[i] - df$meanPre_Int[i]
    }
    if (!isTRUE(single_group) && !any(is.na(c(df$meanPre_Con[i], df$meanPost_Con[i])))) {
      meanDiff_con[i] <- df$meanPost_Con[i] - df$meanPre_Con[i]
    }

    # r inicial (si hay datos)
    if (!any(is.na(c(df$sd_pre_Int[i], df$sd_post_Int[i], sd_diff_int[i])))) {
      r_int[i] <- ((df$sd_pre_Int[i]^2 + df$sd_post_Int[i]^2 - sd_diff_int[i]^2) /
                     (2 * df$sd_pre_Int[i] * df$sd_post_Int[i]))
    }
    if (!isTRUE(single_group) && !any(is.na(c(df$sd_pre_Con[i], df$sd_post_Con[i], sd_diff_con[i])))) {
      r_con[i] <- ((df$sd_pre_Con[i]^2 + df$sd_post_Con[i]^2 - sd_diff_con[i]^2) /
                     (2 * df$sd_pre_Con[i] * df$sd_post_Con[i]))
    }
  }

  # -------------------------
  # PRE-CÁLCULO CV GLOBAL (robusto, sin r asumido)
  # -------------------------
  # INT
  cv_pool_int <- numeric(0)
  known_idx_int <- which(!is.na(sd_diff_int) & !is.na(meanDiff_int) & meanDiff_int != 0)
  if (length(known_idx_int) > 0) {
    cv_pool_int <- c(cv_pool_int, sd_diff_int[known_idx_int] / abs(meanDiff_int[known_idx_int]))
  }
  interval_idx_int <- which(is.na(sd_diff_int) &
                              !is.na(df$sd_pre_Int) & !is.na(df$sd_post_Int) &
                              !is.na(meanDiff_int) & meanDiff_int != 0)
  if (length(interval_idx_int) > 0) {
    preI  <- df$sd_pre_Int[interval_idx_int]
    postI <- df$sd_post_Int[interval_idx_int]
    absdI <- abs(meanDiff_int[interval_idx_int])

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

  # CON
  cv_global_con <- NA_real_
  if (!isTRUE(single_group)) {
    cv_pool_con <- numeric(0)
    known_idx_con <- which(!is.na(sd_diff_con) & !is.na(meanDiff_con) & meanDiff_con != 0)
    if (length(known_idx_con) > 0) {
      cv_pool_con <- c(cv_pool_con, sd_diff_con[known_idx_con] / abs(meanDiff_con[known_idx_con]))
    }
    interval_idx_con <- which(is.na(sd_diff_con) &
                                !is.na(df$sd_pre_Con) & !is.na(df$sd_post_Con) &
                                !is.na(meanDiff_con) & meanDiff_con != 0)
    if (length(interval_idx_con) > 0) {
      preC  <- df$sd_pre_Con[interval_idx_con]
      postC <- df$sd_post_Con[interval_idx_con]
      absdC <- abs(meanDiff_con[interval_idx_con])

      sd_minC <- sqrt(pmax(preC^2 + postC^2 - 2*0.9999*preC*postC, 0))
      sd_maxC <- sqrt(pmax(preC^2 + postC^2 - 2*(-0.9999)*preC*postC, 0))

      cv_minC <- sd_minC / absdC
      cv_maxC <- sd_maxC / absdC

      cv_repC <- sqrt(cv_minC * cv_maxC)
      cv_repC[!is.finite(cv_repC)] <- NA_real_
      cv_pool_con <- c(cv_pool_con, cv_repC)
    }
    cv_global_con <- suppressWarnings(stats::median(cv_pool_con, na.rm = TRUE))
    if (!is.finite(cv_global_con)) cv_global_con <- NA_real_
  }

  # -------------------------
  # BLOQUE: Calcular máximos para imputación "direct"
  # -------------------------
  sd_diff_int_max <- if (any(!is.na(sd_diff_int))) max(sd_diff_int, na.rm = TRUE) else NA_real_
  sd_diff_con_max <- if (any(!is.na(sd_diff_con))) max(sd_diff_con, na.rm = TRUE) else NA_real_

  if (is.na(sd_diff_int_max) && impute_method == "direct") warning("No real SD diff values available to impute (int).")
  if (!isTRUE(single_group) && is.na(sd_diff_con_max) && impute_method == "direct") warning("No real SD diff values available to impute (con).")

  # -------------------------
  # SEGUNDO BUCLE: Imputación de NA con recálculo y warnings de rango r
  # -------------------------
  for (i in seq_len(n)) {
    imputado_int <- FALSE
    imputado_con <- FALSE

    # IMPUTACIÓN MANUAL PREVIA (custom) — INTERVENCIÓN
    if (!is.null(custom_sd_diff_int)) {
      for (entry in custom_sd_diff_int) {
        if (!all(c("row", "value") %in% names(entry))) next
        row_idx <- as.integer(entry$row)
        val <- as.numeric(entry$value)
        if (row_idx == i) {
          sd_diff_int[i] <- val
          imputado_int <- TRUE
          if (verbose) message(sprintf("Custom sd_diff_int set at row %d: %.4f", i, val))
          if (isTRUE(report_imputations)) {
            pre <- df$sd_pre_Int[i]; post <- df$sd_post_Int[i]
            if (!is.na(pre) && !is.na(post)) {
              sd_min <- sqrt(pre^2 + post^2 - 2 * 0.9999 * pre * post)
              sd_max <- sqrt(pre^2 + post^2 - 2 * (-0.9999) * pre * post)
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]],
                                       sprintf("sd_diff_int (manual: %.4f, suggested_range: [%.4f, %.4f])", val, sd_min, sd_max)))
            } else {
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_diff_int (manual: %.4f)", val)))
            }
          }
        }
      }
    }

    # IMPUTACIÓN INTERVENCIÓN (si sigue NA)
    if (is.na(sd_diff_int[i])) {
      if (impute_method == "direct") {
        if (is.finite(sd_diff_int_max)) {
          sd_diff_int[i] <- sd_diff_int_max
          imputado_int <- TRUE
          if (verbose) message(sprintf("Imputed sd_diff_int at row %d using 'direct': %0.4f", i, sd_diff_int[i]))
        }
      } else if (impute_method == "mean") {
        valid_mean <- mean(sd_diff_int, na.rm = TRUE)
        if (is.finite(valid_mean)) {
          sd_diff_int[i] <- valid_mean
          imputado_int <- TRUE
          if (verbose) message(sprintf("Imputed sd_diff_int at row %d using 'mean': %0.4f", i, sd_diff_int[i]))
        }
      } else if (impute_method == "cv") {
        if (is.finite(cv_global_int) && !is.na(meanDiff_int[i]) && meanDiff_int[i] != 0) {
          sd_diff_int[i] <- abs(meanDiff_int[i]) * cv_global_int
          imputado_int <- TRUE
          if (verbose) message(sprintf("Imputed sd_diff_int at row %d using 'cv' (global median): %.4f", i, sd_diff_int[i]))
        }
      }

      # Log imputación sd_diff_int
      if (isTRUE(report_imputations) && imputado_int) {
        if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
        imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_diff_int (%s: %.4f)", impute_method, sd_diff_int[i])))
      }
    }

    # Recalcular r_int tras imputar / custom
    if (!is.na(df$sd_pre_Int[i]) && !is.na(df$sd_post_Int[i]) && !is.na(sd_diff_int[i])) {
      r_temp <- ((df$sd_pre_Int[i]^2 + df$sd_post_Int[i]^2 - sd_diff_int[i]^2) /
                   (2 * df$sd_pre_Int[i] * df$sd_post_Int[i]))
      if (!is.na(r_temp) && (r_temp <= -0.9999 || r_temp >= 0.9999)) {
        sd_min <- sqrt(df$sd_pre_Int[i]^2 + df$sd_post_Int[i]^2 - 2 * 0.9999 * df$sd_pre_Int[i] * df$sd_post_Int[i])
        sd_max <- sqrt(df$sd_pre_Int[i]^2 + df$sd_post_Int[i]^2 - 2 * (-0.9999) * df$sd_pre_Int[i] * df$sd_post_Int[i])
        warning(sprintf(
          paste0("Row %d: Imputed sd_diff_int = %.4f gives r_int = %.4f (outside [-0.9999, 0.9999]).\n",
                 "Suggested sd_diff_int range: [%.4f, %.4f].\n r_int not assigned."),
          i, sd_diff_int[i], r_temp, sd_min, sd_max
        ))
        r_int[i] <- NA_real_
      } else {
        r_int[i] <- r_temp
        if (isTRUE(report_imputations) && imputado_int && !is.na(r_int[i]) && r_int[i] > -0.9999 && r_int[i] < 0.9999) {
          if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
          imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("r_int (%s: %.4f)", impute_method, r_int[i])))
        }
      }
    }

    # IMPUTACIÓN MANUAL PREVIA — CONTROL
    if (!isTRUE(single_group) && !is.null(custom_sd_diff_con)) {
      for (entry in custom_sd_diff_con) {
        if (!all(c("row", "value") %in% names(entry))) next
        row_idx <- as.integer(entry$row)
        val <- as.numeric(entry$value)
        if (row_idx == i) {
          sd_diff_con[i] <- val
          imputado_con <- TRUE
          if (verbose) message(sprintf("Custom sd_diff_con set at row %d: %.4f", i, val))
          if (isTRUE(report_imputations)) {
            pre <- df$sd_pre_Con[i]; post <- df$sd_post_Con[i]
            if (!is.na(pre) && !is.na(post)) {
              sd_min <- sqrt(pre^2 + post^2 - 2 * 0.9999 * pre * post)
              sd_max <- sqrt(pre^2 + post^2 - 2 * (-0.9999) * pre * post)
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]],
                                       sprintf("sd_diff_con (manual: %.4f, suggested_range: [%.4f, %.4f])", val, sd_min, sd_max)))
            } else {
              if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
              imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_diff_con (manual: %.4f)", val)))
            }
          }
        }
      }
    }

    # IMPUTACIÓN CONTROL (si procede y sigue NA)
    if (!isTRUE(single_group) && is.na(sd_diff_con[i])) {
      if (impute_method == "direct") {
        if (is.finite(sd_diff_con_max)) {
          sd_diff_con[i] <- sd_diff_con_max
          imputado_con <- TRUE
          if (verbose) message(sprintf("Imputed sd_diff_con at row %d using 'direct': %0.4f", i, sd_diff_con[i]))
        }
      } else if (impute_method == "mean") {
        valid_mean <- mean(sd_diff_con, na.rm = TRUE)
        if (is.finite(valid_mean)) {
          sd_diff_con[i] <- valid_mean
          imputado_con <- TRUE
          if (verbose) message(sprintf("Imputed sd_diff_con at row %d using 'mean': %0.4f", i, sd_diff_con[i]))
        }
      } else if (impute_method == "cv") {
        if (is.finite(cv_global_con) && !is.na(meanDiff_con[i]) && meanDiff_con[i] != 0) {
          sd_diff_con[i] <- abs(meanDiff_con[i]) * cv_global_con
          imputado_con <- TRUE
          if (verbose) message(sprintf("Imputed sd_diff_con at row %d using 'cv' (global median): %.4f", i, sd_diff_con[i]))
        }
      }

      # Log imputación sd_diff_con
      if (isTRUE(report_imputations) && imputado_con) {
        if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
        imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("sd_diff_con (%s: %.4f)", impute_method, sd_diff_con[i])))
      }
    }

    # Recalcular r_con tras imputar / custom
    if (!isTRUE(single_group) && !any(is.na(c(df$sd_pre_Con[i], df$sd_post_Con[i], sd_diff_con[i])))) {
      r_temp <- ((df$sd_pre_Con[i]^2 + df$sd_post_Con[i]^2 - sd_diff_con[i]^2) /
                   (2 * df$sd_pre_Con[i] * df$sd_post_Con[i]))
      if (!is.na(r_temp) && (r_temp <= -0.9999 || r_temp >= 0.9999)) {
        sd_min <- sqrt(df$sd_pre_Con[i]^2 + df$sd_post_Con[i]^2 - 2 * 0.9999 * df$sd_pre_Con[i] * df$sd_post_Con[i])
        sd_max <- sqrt(df$sd_pre_Con[i]^2 + df$sd_post_Con[i]^2 - 2 * (-0.9999) * df$sd_pre_Con[i] * df$sd_post_Con[i])

        warning(sprintf(
          paste0("Row %d: Imputed sd_diff_con = %.4f gives r_con = %.4f (outside [-0.9999, 0.9999]).\n",
                 "Suggested sd_diff_con range: [%.4f, %.4f].\n r_con not assigned."),
          i, sd_diff_con[i], r_temp, sd_min, sd_max
        ))
        r_con[i] <- NA_real_
      } else {
        r_con[i] <- r_temp
        if (isTRUE(report_imputations) && imputado_con && !is.na(r_con[i]) && r_con[i] > -0.9999 && r_con[i] < 0.9999) {
          if (is.null(imp_log[[i]])) imp_log[[i]] <- character()
          imp_log[[i]] <- unique(c(imp_log[[i]], sprintf("r_con (%s: %.4f)", impute_method, r_con[i])))
        }
      }
    }
  }

  # -------------------------
  # SEGUNDO BLOQUE DE CÁLCULOS: Hedges y tamaños de efecto
  # -------------------------
  for (i in seq_len(n)) {

    # Factor de corrección Hedges (por grupo)
    J_int <- if (apply_hedges && !is.na(df$n_Int[i]) && df$n_Int[i] > 1) 1 - (3 / (4 * (df$n_Int[i] - 1) - 1)) else 1
    J_con <- if (!isTRUE(single_group) && apply_hedges && !is.na(df$n_Con[i]) && df$n_Con[i] > 1) 1 - (3 / (4 * (df$n_Con[i] - 1) - 1)) else 1

    # --- SMDs ---
    if (SMD_method == "SMDpre") {
      if (!is.na(df$sd_pre_Int[i]) && df$sd_pre_Int[i] != 0) {
        SMDpre_int[i] <- J_int * (meanDiff_int[i] / df$sd_pre_Int[i])
        varSMDpre_int[i] <- (J_int^2) * ((2 * (1 - r_int[i])) / df$n_Int[i]) *
          ((df$n_Int[i] - 1)/(df$n_Int[i] - 3)) *
          (1 + (df$n_Int[i] * SMDpre_int[i]^2)/(2 * (1 - r_int[i]))) - SMDpre_int[i]^2
        seSMDpre_int[i] <- sqrt(varSMDpre_int[i])
      }

      if (!isTRUE(single_group) && !is.na(df$sd_pre_Con[i]) && df$sd_pre_Con[i] != 0) {
        SMDpre_con[i] <- J_con * (meanDiff_con[i] / df$sd_pre_Con[i])
        varSMDpre_con[i] <- (J_con^2) * ((2 * (1 - r_con[i])) / df$n_Con[i]) *
          ((df$n_Con[i] - 1)/(df$n_Con[i] - 3)) *
          (1 + (df$n_Con[i] * SMDpre_con[i]^2)/(2 * (1 - r_con[i]))) - SMDpre_con[i]^2
        seSMDpre_con[i] <- sqrt(varSMDpre_con[i])
      }

    } else if (SMD_method == "SMDchange") {
      if (!is.na(sd_diff_int[i]) && sd_diff_int[i] != 0) {
        SMDchange_int[i] <- J_int * (meanDiff_int[i] / sd_diff_int[i])
        varSMDchange_int[i] <- (J_int^2) * (1 / df$n_Int[i]) * ((df$n_Int[i] - 1)/(df$n_Int[i] - 3)) *
          (1 + df$n_Int[i] * SMDchange_int[i]^2) - SMDchange_int[i]^2
        seSMDchange_int[i] <- sqrt(varSMDchange_int[i])
      }
      if (!isTRUE(single_group) && !is.na(sd_diff_con[i]) && sd_diff_con[i] != 0) {
        SMDchange_con[i] <- J_con * (meanDiff_con[i] / sd_diff_con[i])
        varSMDchange_con[i] <- (J_con^2) * (1 / df$n_Con[i]) * ((df$n_Con[i] - 1)/(df$n_Con[i] - 3)) *
          (1 + df$n_Con[i] * SMDchange_con[i]^2) - SMDchange_con[i]^2
        seSMDchange_con[i] <- sqrt(varSMDchange_con[i])
      }

    } else if (SMD_method == "ScMDpooled") {
      if (!isTRUE(single_group) &&
          !any(is.na(c(df$n_Int[i], df$n_Con[i], df$sd_pre_Int[i], df$sd_pre_Con[i],
                       df$meanPre_Int[i], df$meanPost_Int[i], df$meanPre_Con[i], df$meanPost_Con[i],
                       r_int[i], r_con[i])))) {
        pooled_sd <- sqrt(((df$n_Int[i] - 1) * df$sd_pre_Int[i]^2 + (df$n_Con[i] - 1) * df$sd_pre_Con[i]^2) /
                            (df$n_Int[i] + df$n_Con[i] - 2))
        c_factor <- if (apply_hedges) 1 - (3 / (4 * (df$n_Int[i] + df$n_Con[i] - 2) - 1)) else 1
        ScMDpooled[i] <- c_factor * ((df$meanPre_Int[i] - df$meanPost_Int[i]) -
                                       (df$meanPre_Con[i] - df$meanPost_Con[i])) / pooled_sd

        r_m <- (r_int[i] + r_con[i]) / 2
        n1 <- df$n_Int[i]; n2 <- df$n_Con[i]
        varScMDpooled[i] <- (c_factor^2) * 2 * (1 - r_m) * ((n1 + n2)/(n1 * n2)) *
          ((n1 + n2 - 2)/(n1 + n2 - 4)) *
          (1 + ((n1 * n2 * ScMDpooled[i]^2)/(2 * (1 - r_m) * (n1 + n2)))) - ScMDpooled[i]^2
        seScMDpooled[i] <- sqrt(varScMDpooled[i])
      }

    } else if (SMD_method == "ScMDpre") {
      # Efectos individuales
      if (!any(is.na(c(df$meanPre_Int[i], df$meanPost_Int[i], df$sd_pre_Int[i]))) && df$sd_pre_Int[i] != 0) {
        SMDmat_int <- (df$meanPre_Int[i] - df$meanPost_Int[i]) / df$sd_pre_Int[i]
      } else {
        SMDmat_int <- NA_real_
      }
      if (!isTRUE(single_group) && !any(is.na(c(df$meanPre_Con[i], df$meanPost_Con[i], df$sd_pre_Con[i]))) && df$sd_pre_Con[i] != 0) {
        SMDmat_con <- (df$meanPre_Con[i] - df$meanPost_Con[i]) / df$sd_pre_Con[i]
      } else {
        SMDmat_con <- NA_real_
      }

      ScMDpre[i] <- if (!isTRUE(single_group)) SMDmat_int - SMDmat_con else NA_real_

      # Varianzas corregidas por grupo
      if (!is.na(SMDmat_int) && !is.na(r_int[i]) && !is.na(df$n_Int[i]) && df$n_Int[i] > 3) {
        var_SMDmat_int <- (J_int^2) *
          ((2 * (1 - r_int[i])) / df$n_Int[i]) *
          ((df$n_Int[i] - 1) / (df$n_Int[i] - 3)) *
          (1 + (df$n_Int[i] * SMDmat_int^2) / (2 * (1 - r_int[i]))) -
          SMDmat_int^2
      } else var_SMDmat_int <- NA_real_

      if (!isTRUE(single_group) && !is.na(SMDmat_con) && !is.na(r_con[i]) && !is.na(df$n_Con[i]) && df$n_Con[i] > 3) {
        var_SMDmat_con <- (J_con^2) *
          ((2 * (1 - r_con[i])) / df$n_Con[i]) *
          ((df$n_Con[i] - 1) / (df$n_Con[i] - 3)) *
          (1 + (df$n_Con[i] * SMDmat_con^2) / (2 * (1 - r_con[i]))) -
          SMDmat_con^2
      } else var_SMDmat_con <- NA_real_

      var_ScMDpre[i] <- if (!isTRUE(single_group)) var_SMDmat_int + var_SMDmat_con else NA_real_
      se_ScMDpre[i] <- if (!is.na(var_ScMDpre[i])) sqrt(var_ScMDpre[i]) else NA_real_
    }

    # % cambio
    if (!any(is.na(c(meanDiff_int[i], df$meanPre_Int[i]))) && df$meanPre_Int[i] != 0) {
      pct_change_int[i] <- (meanDiff_int[i] / df$meanPre_Int[i]) * 100
    }
    if (!isTRUE(single_group) && !any(is.na(c(meanDiff_con[i], df$meanPre_Con[i]))) && df$meanPre_Con[i] != 0) {
      pct_change_con[i] <- (meanDiff_con[i] / df$meanPre_Con[i]) * 100
    }

    # Diferencias de medias (opcionales)
    if (MeanDifferences) {
      if (!any(is.na(c(sd_diff_int[i], df$n_Int[i]))) && df$n_Int[i] > 0) {
        varMeanDiff_int[i] <- sd_diff_int[i]^2 / df$n_Int[i]
        seMeanDiff_int[i] <- sqrt(varMeanDiff_int[i])
      }
      if (!isTRUE(single_group) && !any(is.na(c(sd_diff_con[i], df$n_Con[i]))) && df$n_Con[i] > 0) {
        varMeanDiff_con[i] <- sd_diff_con[i]^2 / df$n_Con[i]
        seMeanDiff_con[i] <- sqrt(varMeanDiff_con[i])
      }
    }

    # Redondeo por fila (si se solicita)
    if (!is.null(digits)) {
      vars_to_round <- c("r_int", "r_con",
                         "SMDpre_int", "SMDpre_con", "varSMDpre_int", "varSMDpre_con", "seSMDpre_int", "seSMDpre_con",
                         "SMDchange_int", "SMDchange_con", "varSMDchange_int", "varSMDchange_con", "seSMDchange_int", "seSMDchange_con",
                         "ScMDpooled", "varScMDpooled", "seScMDpooled",
                         "meanDiff_int", "meanDiff_con", "varMeanDiff_int", "varMeanDiff_con", "seMeanDiff_int", "seMeanDiff_con",
                         "pct_change_int", "pct_change_con", "ScMDpre", "var_ScMDpre", "se_ScMDpre", "sd_diff_int", "sd_diff_con")
      for (varname in vars_to_round) {
        # usa get/assign dentro del entorno local
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

  # -------------------------
  # BLOQUE: Generación de informe Word de imputaciones
  # -------------------------
  if (isTRUE(report_imputations)) {
    style_normal <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    style_bold   <- officer::fp_text(font.size = 12, font.family = "Times New Roman", bold = TRUE)
    style_sub    <- officer::fp_text(font.size = 12, font.family = "Times New Roman", vertical.align = "subscript")

    doc <- officer::read_docx()
    doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("Imputation Report", style_bold)))
    doc <- officer::body_add_par(doc, "", style = "Normal")

    n_sd_int <- sum(grepl("^sd_diff_int", unlist(imp_log)))
    n_sd_con <- sum(grepl("^sd_diff_con", unlist(imp_log)))
    n_r_int  <- sum(grepl("^r_int", unlist(imp_log)))
    n_r_con  <- sum(grepl("^r_con", unlist(imp_log)))
    n_imputaciones <- n_sd_int + n_sd_con + n_r_int + n_r_con

    if (isTRUE(single_group)) {
      doc <- officer::body_add_fpar(doc, officer::fpar(
        officer::ftext(sprintf("A total of %d imputations were performed: ", n_imputaciones), style_normal),
        officer::ftext(sprintf("%d for SD", n_sd_int), style_normal),
        officer::ftext("diff", style_sub),
        officer::ftext(" (intervention), ", style_normal),
        officer::ftext(sprintf("%d for r (intervention).", n_r_int), style_normal)
      ))
    } else {
      doc <- officer::body_add_fpar(doc, officer::fpar(
        officer::ftext(sprintf("A total of %d imputations were performed: %d for SD", n_imputaciones, n_sd_int), style_normal),
        officer::ftext("diff", style_sub),
        officer::ftext(" (intervention), ", style_normal),
        officer::ftext(sprintf("%d for SD", n_sd_con), style_normal),
        officer::ftext("diff", style_sub),
        officer::ftext(" (control), ", style_normal),
        officer::ftext(sprintf("%d for r (intervention), %d for r (control).", n_r_int, n_r_con), style_normal)
      ))
    }

    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("Missing values were identified for the standard deviation of the difference scores (", style_normal),
      officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
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
          # Intervention group - SDdiff imputations
          logs_sd_int <- imp_log[[i]][grepl("^sd_diff_int", imp_log[[i]])]
          for (log in logs_sd_int) {
            matches_manual_range <- stringr::str_match(
              log, "sd_diff_int \\(manual: ([0-9.]+), suggested_range: \\[([0-9.eE+-]+), ([0-9.eE+-]+)\\]\\)"
            )
            if (!any(is.na(matches_manual_range))) {
              valor <- as.numeric(matches_manual_range[2])
              rango_min <- as.numeric(matches_manual_range[3])
              rango_max <- as.numeric(matches_manual_range[4])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
                officer::ftext(" for the intervention group was manually entered as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(" (suggested ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
                officer::ftext(paste0(" range: [", sprintf("%.2f", rango_min), ", ", sprintf("%.2f", rango_max), "])."), style_normal)
              ))
              next
            }
            matches_manual <- stringr::str_match(log, "sd_diff_int \\(manual: ([0-9.]+)\\)")
            if (!any(is.na(matches_manual))) {
              valor <- as.numeric(matches_manual[2])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
                officer::ftext(paste0(" for the intervention group was manually entered as ", sprintf("%.2f", valor), "."), style_normal)
              ))
              next
            }
            matches <- stringr::str_match(log, "sd_diff_int \\((\\w+): ([0-9.\\-]+)\\)")
            if (!any(is.na(matches))) {
              metodo <- matches[2]; valor <- as.numeric(matches[3])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The SD", style_normal),
                officer::ftext("diff", style_sub),
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
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
                officer::ftext(".", style_normal)
              ))
            }
          }
          # Control group - SDdiff imputations
          logs_sd_con <- imp_log[[i]][grepl("^sd_diff_con", imp_log[[i]])]
          for (log in logs_sd_con) {
            matches_manual_range <- stringr::str_match(
              log, "sd_diff_con \\(manual: ([0-9.]+), suggested_range: \\[([0-9.eE+-]+), ([0-9.eE+-]+)\\]\\)"
            )
            if (!any(is.na(matches_manual_range))) {
              valor <- as.numeric(matches_manual_range[2])
              rango_min <- as.numeric(matches_manual_range[3])
              rango_max <- as.numeric(matches_manual_range[4])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
                officer::ftext(" for the control group was manually entered as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(" (suggested ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
                officer::ftext(paste0(" range: [", sprintf("%.2f", rango_min), ", ", sprintf("%.2f", rango_max), "])."), style_normal)
              ))
              next
            }
            matches_manual <- stringr::str_match(log, "sd_diff_con \\(manual: ([0-9.]+)\\)")
            if (!any(is.na(matches_manual))) {
              valor <- as.numeric(matches_manual[2])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The ", style_normal),
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
                officer::ftext(paste0(" for the control group was manually entered as ", sprintf("%.2f", valor), "."), style_normal)
              ))
              next
            }
            matches <- stringr::str_match(log, "sd_diff_con \\((\\w+): ([0-9.\\-]+)\\)")
            if (!any(is.na(matches))) {
              metodo <- matches[2]; valor <- as.numeric(matches[3])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The SD", style_normal),
                officer::ftext("diff", style_sub),
                officer::ftext(" for the control group was imputed as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(sprintf(" using the %s method.", metodo), style_normal)
              ))
            }
          }
          # Control group - r_con imputations
          logs_r_con <- imp_log[[i]][grepl("^r_con", imp_log[[i]])]
          for (log in logs_r_con) {
            matches_manual <- stringr::str_match(log, "r_con \\(manual: ([0-9.\\-]+)\\)")
            if (!any(is.na(matches_manual))) {
              valor <- as.numeric(matches_manual[2])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The r for the control group was manually entered as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(".", style_normal)
              ))
              next
            }
            matches <- stringr::str_match(log, "r_con \\((\\w+): ([0-9.\\-]+)\\)")
            if (!any(is.na(matches))) {
              metodo <- matches[2]; valor <- as.numeric(matches[3])
              doc <- officer::body_add_fpar(doc, officer::fpar(
                officer::ftext(" The r for the control group was estimated as ", style_normal),
                officer::ftext(sprintf("%.2f", valor), style_bold),
                officer::ftext(sprintf(", using the %s method, based on the imputed ", metodo), style_normal),
                officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
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
      officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
      officer::ftext(" values were missing, they were estimated based on available data from other studies included in the meta-analysis.", style_normal)
    ))
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(doc, officer::fpar(
      officer::ftext("The mean method substituted missing values with the average ", style_normal),
      officer::ftext("SD", style_normal), officer::ftext("diff", style_sub),
      officer::ftext(" across valid cases. The cv method used |mean difference| multiplied by a robust global median of cv obtained from cases with known SD", style_normal),
      officer::ftext("diff", style_sub),
      officer::ftext(" or feasible intervals. The direct method used the maximum available SD", style_normal),
      officer::ftext("diff", style_sub),
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
      officer::ftext("https://training.cochrane.org/handbook", style_normal)
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

  # -------------------------
  # BLOQUE: Construcción final del data.frame de salida
  # -------------------------
  if (add_to_df) {
    df$r_int <- r_int
    df$sd_diff_int <- sd_diff_int
    df$pct_change_int <- pct_change_int

    if (!isTRUE(single_group)) {
      df$r_con <- r_con
      df$sd_diff_con <- sd_diff_con
      df$pct_change_con <- pct_change_con
    }

    if (SMD_method == "SMDpre") {
      df$SMDpre_int <- SMDpre_int
      df$varSMDpre_int <- varSMDpre_int
      df$seSMDpre_int <- seSMDpre_int
      if (!isTRUE(single_group)) {
        df$SMDpre_con <- SMDpre_con
        df$varSMDpre_con <- varSMDpre_con
        df$seSMDpre_con <- seSMDpre_con
      }
    }

    if (SMD_method == "SMDchange") {
      df$SMDchange_int <- SMDchange_int
      df$varSMDchange_int <- varSMDchange_int
      df$seSMDchange_int <- seSMDchange_int
      if (!isTRUE(single_group)) {
        df$SMDchange_con <- SMDchange_con
        df$varSMDchange_con <- varSMDchange_con
        df$seSMDchange_con <- seSMDchange_con
      }
    }

    if (SMD_method == "ScMDpooled" && !isTRUE(single_group)) {
      df$ScMDpooled <- ScMDpooled
      df$varScMDpooled <- varScMDpooled
      df$seScMDpooled <- seScMDpooled
    }

    if (SMD_method == "ScMDpre") {
      df$ScMDpre <- ScMDpre
      df$varScMDpre <- var_ScMDpre
      df$seScMDpre <- se_ScMDpre
    }

    if (MeanDifferences) {
      df$meanDiff_int <- meanDiff_int
      df$varMeanDiff_int <- varMeanDiff_int
      df$seMeanDiff_int <- seMeanDiff_int
      if (!isTRUE(single_group)) {
        df$meanDiff_con <- meanDiff_con
        df$varMeanDiff_con <- varMeanDiff_con
        df$seMeanDiff_con <- seMeanDiff_con
      }
    }

    # Eliminar columnas dummy del control si quedaron totalmente NA
    dummy_cols <- c("meanPre_Con", "meanPost_Con", "sd_pre_Con", "sd_post_Con",
                    "upperCI_Con", "lowerCI_Con", "n_Con", "p_value_Con")
    for (col in dummy_cols) {
      if (all(is.na(df[[col]]))) {
        df[[col]] <- NULL
      }
    }

    return(df)
  }
}


