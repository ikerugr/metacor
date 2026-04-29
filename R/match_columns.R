# Internal: resolve user-supplied column names to metacor's canonical schema.
#
# Two layers, in order:
#   1. Normalisation (lowercase + strip separators). Cannot produce false
#      positives — only collapses formatting differences.
#   2. Closed synonym lookup against the dictionaries in `R/synonyms.R`.
#      Only listed strings match. No fuzzy / Levenshtein matching.
#
# See `naming_decisions.md` §6 for the design rationale.
#
# This file deliberately exports nothing: the matcher is internal
# infrastructure used by `metacor_dual()` (wired in a later step).

# ---------------------------------------------------------------------------
# Public entry point (internal to the package; @noRd)
# ---------------------------------------------------------------------------

#' Resolve user column names to metacor's canonical schema (internal)
#'
#' @param df Data frame whose column names will be resolved.
#' @param single_group Logical. If TRUE, columns without a group suffix are
#'   interpreted as belonging to the intervention group (`_int`), and any
#'   column resolving to a control-group column is dropped with a warning.
#' @param mode `"flexible"` (default) applies normalisation + synonym lookup.
#'   `"strict"` returns the data frame unchanged so the caller's downstream
#'   logic enforces canonical names.
#' @return A list with elements:
#'   * `df` — the data frame with renamed columns.
#'   * `mapping` — `data.frame(user_name, canonical, kind)` describing every
#'     resolution; `kind` is one of `"exact"`, `"case_only"`, `"synonym"`,
#'     `"legacy_v12"`.
#'   * `messages` — character vector of informational messages (synonym matches).
#'   * `warnings` — character vector of deprecation warnings (legacy_v12
#'     matches and dropped `_ctrl` columns under `single_group = TRUE`).
#' @noRd
match_columns <- function(df, single_group = FALSE,
                          mode = c("flexible", "strict")) {
  mode <- match.arg(mode)

  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  user_cols <- names(df)

  if (length(user_cols) == 0L || mode == "strict") {
    return(list(df = df,
                mapping = empty_mapping(),
                messages = character(),
                warnings = character()))
  }

  # 1. Resolve each column independently. Errors here are user-visible
  #    ("ambiguous column").
  resolutions <- lapply(user_cols, resolve_column, single_group = single_group)

  # 2. Detect cross-column ambiguity: two user columns resolving to the same
  #    canonical name.
  detect_canonical_collisions(user_cols, resolutions)

  # 3. Build the renamed data frame and the resolution log.
  df_out <- df
  mapping_rows <- list()
  msgs <- character()
  warns <- character()
  drop_cols <- integer()

  for (i in seq_along(resolutions)) {
    r <- resolutions[[i]]
    if (is.null(r$canonical)) next  # passthrough: not a metacor concept

    if (single_group && isTRUE(r$is_ctrl)) {
      drop_cols <- c(drop_cols, i)
      warns <- c(warns, sprintf(
        "Column '%s' looks like a control-group column (resolves to '%s'); ignored under single_group = TRUE.",
        user_cols[i], r$canonical
      ))
      next
    }

    if (!identical(user_cols[i], r$canonical)) {
      names(df_out)[i] <- r$canonical
    }

    mapping_rows[[length(mapping_rows) + 1L]] <- data.frame(
      user_name = user_cols[i],
      canonical = r$canonical,
      kind      = r$kind,
      stringsAsFactors = FALSE
    )

    if (identical(r$kind, "synonym")) {
      msgs <- c(msgs, sprintf("  * '%s' -> %s", user_cols[i], r$canonical))
    } else if (identical(r$kind, "legacy_v12")) {
      warns <- c(warns, sprintf(
        "Column '%s' uses the v1.2.x name and will stop being recognised in metacor 2.0.0; please rename to '%s'.",
        user_cols[i], r$canonical
      ))
    }
    # "exact" and "case_only" resolutions are silent.
  }

  if (length(drop_cols) > 0L) {
    df_out <- df_out[, -drop_cols, drop = FALSE]
  }

  mapping <- if (length(mapping_rows) == 0L) {
    empty_mapping()
  } else {
    do.call(rbind, mapping_rows)
  }

  # Add a header to the messages block when there were any synonym matches.
  flex_n <- if (nrow(mapping) > 0L) sum(mapping$kind == "synonym") else 0L
  if (flex_n > 0L) {
    msgs <- c(
      sprintf("Matched %d column(s) flexibly to canonical metacor names:",
              flex_n),
      msgs,
      "Pass column_matching = \"strict\" to disable flexible matching."
    )
  }

  list(df = df_out, mapping = mapping, messages = msgs, warnings = warns)
}

# ---------------------------------------------------------------------------
# Per-column resolution (no side effects)
# ---------------------------------------------------------------------------

# Returns either:
#   list(canonical = NULL)                                  -> passthrough
#   list(canonical = <chr>, kind = <chr>, is_ctrl = <lgl>)  -> resolved
# Throws an error on intra-column ambiguity (one input matching two canonicals).
resolve_column <- function(user_name, single_group) {
  norm <- normalize_name(user_name)

  # 1. Passthrough concepts (no group suffix expected).
  for (can in names(PASSTHROUGH_SYNONYMS)) {
    set <- PASSTHROUGH_SYNONYMS[[can]]
    if (norm %in% set$synonyms) {
      return(list(
        canonical = can,
        kind      = classify_kind(user_name, can,
                                  is_legacy = norm %in% set$legacy),
        is_ctrl   = FALSE
      ))
    }
  }

  # 2. (stem, suffix) splits. Try suffixes longest-first so that
  #    `intervention` is preferred over the shorter `int` when both
  #    could syntactically match.
  all_suffix_synonyms <- unique(unlist(
    lapply(GROUP_SUFFIX_SYNONYMS, function(x) x$synonyms)
  ))
  all_suffix_synonyms <- all_suffix_synonyms[
    order(-nchar(all_suffix_synonyms))
  ]

  candidates <- list()
  for (suf_norm in all_suffix_synonyms) {
    if (!ends_with(norm, suf_norm)) next
    stem_norm <- substr(norm, 1L, nchar(norm) - nchar(suf_norm))
    if (nchar(stem_norm) == 0L) next  # column was just the suffix

    suf_canonical <- canonical_for_suffix(suf_norm)
    if (is.null(suf_canonical)) next

    for (stem_can in names(STEM_SYNONYMS)) {
      stem_set <- STEM_SYNONYMS[[stem_can]]
      if (stem_norm %in% stem_set$synonyms) {
        candidates[[length(candidates) + 1L]] <- list(
          stem_canonical   = stem_can,
          suffix_canonical = suf_canonical,
          stem_norm        = stem_norm,
          suffix_norm      = suf_norm,
          stem_legacy_set  = stem_set$legacy,
          suffix_legacy_set = GROUP_SUFFIX_SYNONYMS[[suf_canonical]]$legacy
        )
      }
    }
  }

  # 3. Single-group fallback: a column with no recognised suffix is
  #    interpreted as `_int`.
  if (single_group && length(candidates) == 0L) {
    for (stem_can in names(STEM_SYNONYMS)) {
      stem_set <- STEM_SYNONYMS[[stem_can]]
      if (norm %in% stem_set$synonyms) {
        candidates[[length(candidates) + 1L]] <- list(
          stem_canonical   = stem_can,
          suffix_canonical = "int",
          stem_norm        = norm,
          suffix_norm      = "",
          stem_legacy_set  = stem_set$legacy,
          suffix_legacy_set = character()
        )
      }
    }
  }

  if (length(candidates) == 0L) {
    return(list(canonical = NULL))
  }

  # Reduce to unique resolved canonicals (longest-suffix-first ordering can
  # produce duplicates if two suffix synonyms map to the same canonical).
  uniq <- unique(vapply(candidates, function(c)
    paste(c$stem_canonical, c$suffix_canonical, sep = "_"),
    character(1)))

  if (length(uniq) > 1L) {
    stop(sprintf(
      "Column '%s' is ambiguous: could be %s. Rename it explicitly or pass column_matching = \"strict\".",
      user_name, paste(uniq, collapse = " or ")),
      call. = FALSE)
  }

  best <- candidates[[1L]]
  canonical_full <- paste(best$stem_canonical, best$suffix_canonical, sep = "_")

  is_legacy <- (best$stem_norm %in% best$stem_legacy_set) ||
               (best$suffix_norm %in% best$suffix_legacy_set)

  list(
    canonical = canonical_full,
    kind      = classify_kind(user_name, canonical_full, is_legacy),
    is_ctrl   = identical(best$suffix_canonical, "ctrl")
  )
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Lowercase + strip the four separator classes we recognise.
normalize_name <- function(x) {
  tolower(gsub("[_.[:space:]\\-]", "", x, perl = TRUE))
}

ends_with <- function(x, suf) {
  nx <- nchar(x); ns <- nchar(suf)
  if (ns > nx) return(FALSE)
  identical(substr(x, nx - ns + 1L, nx), suf)
}

canonical_for_suffix <- function(suf_norm) {
  for (can in names(GROUP_SUFFIX_SYNONYMS)) {
    if (suf_norm %in% GROUP_SUFFIX_SYNONYMS[[can]]$synonyms) return(can)
  }
  NULL
}

# Classify a resolved match as one of:
#   - "exact"      user wrote exactly the canonical
#   - "case_only"  user differed only in case / separators
#   - "legacy_v12" user used a v1.2.x deprecated name
#   - "synonym"    user used a non-legacy alternative spelling
classify_kind <- function(user_name, canonical, is_legacy) {
  if (isTRUE(is_legacy)) return("legacy_v12")
  if (identical(user_name, canonical)) return("exact")
  if (identical(normalize_name(user_name), normalize_name(canonical))) {
    return("case_only")
  }
  "synonym"
}

empty_mapping <- function() {
  data.frame(user_name = character(), canonical = character(),
             kind = character(), stringsAsFactors = FALSE)
}

detect_canonical_collisions <- function(user_cols, resolutions) {
  canonicals <- vapply(resolutions, function(r) {
    if (is.null(r$canonical)) NA_character_ else r$canonical
  }, character(1))

  dups <- unique(canonicals[duplicated(canonicals) & !is.na(canonicals)])
  if (length(dups) == 0L) return(invisible(NULL))

  lines <- vapply(dups, function(can) {
    idx <- which(canonicals == can)
    sprintf("  * %s <- {%s}", can, paste(user_cols[idx], collapse = ", "))
  }, character(1))

  stop(sprintf(
    "Column matching is ambiguous: multiple input columns map to the same canonical column.\n%s\nRename the offending columns or pass column_matching = \"strict\".",
    paste(lines, collapse = "\n")),
    call. = FALSE)
}
