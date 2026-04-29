# Force lifecycle deprecation warnings to fire on every call.
#
# By default lifecycle deduplicates a given `deprecate_warn()` to once per
# session, which makes test loops over multiple deprecated values miss the
# warning on every iteration after the first. Setting verbosity to
# "warning" disables the dedup for the duration of the test run.

options(lifecycle_verbosity = "warning")
