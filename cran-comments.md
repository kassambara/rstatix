## Test environments
* local macOS, R 4.5.x
* GitHub Actions (check-standard): macOS-release, Windows-release,
  Ubuntu-latest (devel, release, oldrel-1), plus an Ubuntu job run with
  `_R_CHECK_DEPENDS_ONLY_=true` (Suggests absent)
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes.

## Minor release: 1.1.0

Previous CRAN version: 1.0.0. This release is additive apart from one deliberate
change, called out here:

* `cramer_v()` no longer applies Yates' continuity correction by default
  (`correct` now defaults to `FALSE`). Yates' correction belongs to the
  chi-square test, not to an effect size, where it only biases Cramer's V
  downward. This changes the value returned for 2x2 tables (the only shape it
  affects): the function now returns the standard `sqrt(chi2 / (N * (k - 1)))`,
  matching `DescTools::CramerV()` and `effectsize::cramers_v(adjust = FALSE)`.
  Passing `correct = TRUE` recovers the previous value. Larger tables are
  unaffected. See NEWS.md and issue #293.

Everything else is backward compatible: new functions (`cliff_delta()`,
`omega_squared()`/`partial_omega_squared()`, `check_test_assumptions()`,
`posthoc_test()`, `tidy()`/`glance()` methods for `rstatix_test` objects), and
new arguments whose defaults reproduce the previous output (`effect.size` on the
pairwise tests, `id` on `cohens_d()`, `ci` on `eta_squared()`/`cramer_v()`,
`method` on `wilcox_effsize()`/`kruskal_effsize()`, `style` on
`get_test_label()`, and `boot.parallel`/`boot.ncpus` on the effect-size
functions).

## Reverse dependencies

rstatix has 44 reverse dependencies (Depends/Imports/Suggests). The only change
that alters the output of a pre-existing function for unchanged inputs is the
`cramer_v()` default above; every other change is a new function or a new
argument with an unchanged default, so existing revdep code gets identical
results. We scanned the sources of all 44 reverse dependencies for callers of
`cramer_v()`: one package (BiostatsUHNplus) imports it, and uses the value to
populate an effect-size column whose tests check the output structure rather
than the numeric value, so its checks are unaffected. No new problems are
expected in reverse dependencies.

## Notes
* `datanovia.com` (linked from the documentation) can return HTTP 503 to
  automated crawlers but is a valid, reachable site.
