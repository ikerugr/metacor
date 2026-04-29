# Test data fixtures

These RDS files are inputs for the golden tests in `tests/testthat/`.

## example_single_group.rds

Extracted from `paper/metacor analysis Paper/Example Metacor Paper.xlsx`,
sheet `Sheet1 (2)`, with column names renamed to the v1.3 canonical
schema (`mean_pre_int`, `sd_pre_int`, ...). This is the dataset used in
the `metacor` manuscript. Single-group pre-post design (intervention
only). 11 studies.

Stored as .rds (not .csv) so all-NA numeric columns keep their `numeric`
type instead of round-tripping through `logical`.

Regenerate with `Rscript tools/build_golden_fixtures.R`.

## example_two_group.rds

**Synthetic** dataset hand-crafted to exercise the control-group branch of
`metacor_dual()`. Not real data; do not cite. Designed to cover rows with
p-values only, CIs only, both, and neither (to force imputation). Already
in v1.3 canonical schema.

Regenerate with `Rscript tools/build_golden_fixtures.R`.
