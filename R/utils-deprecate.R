# Deprecation helpers for the v1.2.x -> v1.3.0 transition.
#
# Argument-name shims live inline in metacor_dual() (one-liners using
# `lifecycle::is_present()`); putting them in a helper would obscure the
# control flow more than it would dedupe.
#
# This file holds *value-level* translations — for arguments where the
# accepted set of values changed between versions.

# Translate v1.2.x effect_size strings ("SMDpre", "SMDchange",
# "ScMDpooled", "ScMDpre") to v1.3 strings ("smd_pre", "smd_change",
# "smd_pooled", "smd_diff_groups"). Emits a deprecation warning when a
# legacy value is supplied.
#
# Anything else (NULL, NA, an already-canonical value, garbage) is
# returned unchanged so that downstream `match.arg()` validation produces
# the usual informative error.
translate_effect_size_value <- function(x) {
  legacy_map <- c(
    "SMDpre"     = "smd_pre",
    "SMDchange"  = "smd_change",
    "ScMDpooled" = "smd_pooled",
    "ScMDpre"    = "smd_diff_groups"
  )
  if (length(x) != 1L) return(x)
  if (is.na(x))        return(x)
  if (!is.character(x)) return(x)
  if (!(x %in% names(legacy_map))) return(x)

  new <- unname(legacy_map[x])
  lifecycle::deprecate_warn(
    when = "1.3.0",
    what = sprintf("metacor_dual(effect_size = '%s')", x),
    with = sprintf("metacor_dual(effect_size = '%s')", new)
  )
  new
}
