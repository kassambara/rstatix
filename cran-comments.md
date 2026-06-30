## Test environments
* local macOS, R 4.5.x
* GitHub Actions (check-standard): macOS-release, Windows-release,
  Ubuntu-latest (devel, release, oldrel-1)
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes.

## Major release: 1.0.0

This is a major release (previous CRAN version: 0.7.3). It bundles a large set
of new functions, new arguments and bug fixes accumulated over the development
cycle (see NEWS.md). The version is bumped to 1.0.0 because one change is not
fully backward compatible: test functions now return **full-precision p-values**
(previously rounded to 3 significant figures), so stored/printed `p`/`p.adj`
values gain digits and pairwise adjusted p-values can shift slightly. All other
changes are additive and preserve existing behaviour.

Highlights:
* New functions: conover_test(), friedman_conover_test(), friedman_nemenyi_test(),
  fligner_test(), dunnett_test(), ks_test(), add_cld().
* New arguments: error.as.na (t_test/wilcox_test), id (paired tests),
  ci (anova_test, partial eta-squared confidence intervals).
* freq_table() now supports grouped data.
* Many documentation additions and clearer error messages.

## Reverse dependencies

rstatix has 33 strong reverse dependencies (43 including Suggests). Reverse
dependency checks (revdepcheck) are in progress; the summary ("checked N
revdeps; no new problems" or any breakages + maintainer notifications) will be
recorded here before submission.
