## Resubmission

This is a resubmission of rstatix 1.0.0, addressing the reverse-dependency
issues found in the CRAN incoming check.

* GimmeMyStats and GimmeMyPlot dispatch on the second element of an `anova_test()`
  result's class vector (`method <- sub("_test", "", class(x)[2])`). The 1.0.0
  dplyr/vctrs class-order fix (#106) had shifted the specific "anova_test" class
  out of position 2, so `class(x)[2]` became "rstatix_test" and their dispatch
  fell through. This version restores the order to
  `c("rstatix_test", "anova_test", "data.frame")` -- "rstatix_test" first
  (consistent with every other rstatix test and still before "data.frame"),
  the specific class back in position 2 -- so both packages pass again. A
  regression test locks the class order; `inherits()`, `print()`/`plot()`
  dispatch, dplyr verbs and ggpubr are unaffected.

* microdiluteR's test hard-codes a sign-test p-value rounded to 3 significant
  figures (0.0156); the documented full-precision p-value change (#108) returns
  0.015625. This is the intended, documented behaviour of this major version,
  not a defect in rstatix. The maintainer has been notified and a one-line
  downstream PR has been opened.

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

The principal reverse dependency, ggpubr, was checked against this version and
passes its full test suite (including the coordinated update for issue #153).
The three reverse dependencies flagged by the CRAN incoming check are addressed:
GimmeMyStats and GimmeMyPlot are fixed by the class-order correction in this
resubmission (see above); microdiluteR's failure is the expected full-precision
p-value change (#108), for which a one-line downstream fix has been filed with
its maintainer. The API changes in this release are additive; the one
not-fully-backward-compatible change (full-precision p-values, #108) only adds
digits to returned p-values and does not change function signatures or output
structure.
