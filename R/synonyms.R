# Closed synonym dictionary for flexible column matching.
#
# This file defines the *only* alternative names that match_columns() will
# accept when resolving user-supplied column names against metacor's
# canonical schema. Keep it in sync with §6.2 of `naming_decisions.md`.
#
# Conventions
# -----------
# - All synonym strings are stored *already normalised*: lowercase, with
#   separators (`_`, `-`, `.`, whitespace) stripped. User column names are
#   normalised by `normalize_name()` (in `R/match_columns.R`) before being
#   looked up here.
# - `synonyms` is the closed set of accepted spellings, including the
#   normalised form of the canonical name itself.
# - `legacy` is the subset of `synonyms` that was the canonical name in
#   metacor 1.2.x and therefore deserves a *deprecation warning* (rather
#   than an informational match message).
# - Anything not listed here will *not* be matched. We deliberately do
#   not implement Levenshtein / fuzzy matching: silently confusing
#   `sd_pre` with `sd_post` would be worse than a missing-column error.
#
# Excluded by design
# ------------------
# Single-letter or two-letter shorthands such as `p`, `c`, `e`, `i`, `t`
# are NOT included. They collide with t-statistics, generic group letters,
# and CI columns.

# Stem synonyms: the conceptual "what is this column about" part.
STEM_SYNONYMS <- list(
  mean_pre  = list(synonyms = c("meanpre", "premean", "mpre",
                                "meanbaseline", "baselinemean", "pre",
                                "m1", "mean1"),
                   legacy   = character()),
  mean_post = list(synonyms = c("meanpost", "postmean", "mpost",
                                "meanfollowup", "followupmean", "post",
                                "m2", "mean2"),
                   legacy   = character()),
  sd_pre    = list(synonyms = c("sdpre", "sdbaseline", "sd1", "spre"),
                   legacy   = character()),
  sd_post   = list(synonyms = c("sdpost", "sdfollowup", "sd2", "spost"),
                   legacy   = character()),
  # The big one: SD of pre-post change scores. v1.2.x called this `sd_diff`
  # in arguments / columns and sometimes `SDchange` in the manuscript; v1.3
  # standardises on `sd_change`.
  sd_change = list(synonyms = c("sdchange", "sddiff", "sddifference",
                                "sdd", "sdc", "sdofchange"),
                   legacy   = c("sddiff")),
  n         = list(synonyms = c("n", "ntotal", "samplesize", "nsubjects"),
                   legacy   = character()),
  p_value   = list(synonyms = c("pvalue", "pval", "sig"),
                   legacy   = character()),
  upper_ci  = list(synonyms = c("upperci", "ciupper", "ub",
                                "upperbound", "cihigh"),
                   legacy   = character()),
  lower_ci  = list(synonyms = c("lowerci", "cilower", "lb",
                                "lowerbound", "cilow"),
                   legacy   = character())
)

# Group suffix synonyms: the "intervention vs control" part.
# Note that `_Int` (the v1.2.x form) collapses to `int` after normalisation
# and therefore matches the canonical name directly — no deprecation
# warning is needed for capitalisation alone. `_Con`, however, normalises
# to `con`, which is a *different* word from the new canonical `ctrl`,
# so it does deserve a deprecation warning.
GROUP_SUFFIX_SYNONYMS <- list(
  int  = list(synonyms = c("int", "intervention", "treatment", "trt", "exp"),
              legacy   = character()),
  ctrl = list(synonyms = c("ctrl", "control", "con", "placebo", "pbo",
                           "sham", "passive"),
              legacy   = c("con"))
)

# Passthrough columns: not group-specific, no suffix expected.
PASSTHROUGH_SYNONYMS <- list(
  study_name = list(synonyms = c("studyname", "study", "paper", "reference"),
                    legacy   = character())
)
