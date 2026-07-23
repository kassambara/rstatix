# rstatix 1.1.0

## New features

- `cohens_d()` gains a `ci.method` argument choosing how the confidence interval is computed when `ci = TRUE`. The new `ci.method = "analytic"` returns a deterministic interval obtained by inverting the noncentral *t* distribution (Steiger, 2004; Cumming & Finch, 2001) — the same approach `anova_test(ci = )` uses for partial eta-squared — for the one-sample, paired and two-sample (equal-variance and Welch) cases, and scales it by the Hedges factor when `hedges.correction = TRUE`. Unlike the bootstrap it needs no seed and is reproducible across runs, and it matches `effectsize::cohens_d(ci = )`; with `hedges.correction = TRUE` the estimate and both bounds are scaled by the documented `(N - 3)/(N - 2.25)` approximation, so they are not identical to `effectsize::hedges_g(ci = )`, which uses the exact gamma-function correction; the gap is proportional to the effect size and grows as the samples shrink. The default `ci.method = "boot"` (percentile bootstrap) is unchanged, so existing `cohens_d(ci = TRUE)` results are the same as before.

- `wilcox_effsize()` and `kruskal_effsize()` gain a `method` argument offering an alternative rank-based effect size. `wilcox_effsize(method = "rank_biserial")` reports the rank-biserial correlation — Cliff's delta for an independent test (equal to `cliff_delta()`) or the matched-pairs rank-biserial for a paired test — instead of the default `r = Z / sqrt(N)`. `kruskal_effsize(method = "epsilon2")` reports the rank epsilon-squared `H / (N - 1)` (matching `effectsize::rank_epsilon_squared()`) instead of the default bias-corrected eta-squared. The defaults are unchanged.

- `check_test_assumptions()` checks the assumptions of a one-way, independent-groups design (normality per group with Shapiro-Wilk, homogeneity of variance with Levene) and returns a tidy one-row recommendation: the two p-values, the verdicts, and the omnibus + post-hoc test they imply (`anova_test()` + `tukey_hsd()`, `welch_anova_test()` + `games_howell_test()`, or `kruskal_test()` + `dunn_test()`). The same single check can then drive both the omnibus and the post-hoc coherently; its result can be passed to `posthoc_test()` through the new `.assumptions` argument to avoid re-running the checks, and `posthoc_test()` also gains an `omnibus` argument that warns if the chosen post-hoc belongs to a different family than an omnibus test already run. The documentation notes the known cost of choosing a test by first testing its assumptions on the same data.

- `posthoc_test()` chooses and runs the post-hoc test appropriate to a one-way, independent-groups design (`outcome ~ group`), following the standard decision tree: it checks normality (Shapiro-Wilk per group) and homogeneity of variance (Levene), then runs Tukey HSD when both hold, Games-Howell when the groups are normal but variances differ, and Dunn's test when the data are not normal. It returns the chosen test's usual pairwise table, with the selected method and the assumption verdicts attached and shown when the result is printed, so the routing is transparent.

- `get_test_label()` and `create_test_label()` gain a `style` argument. With `style = "apa"` the label follows the APA-7 in-text reporting format: the leading test-name label is dropped, the p-value is written APA-style (`p = .023`, `p < .001`), and the effect size is appended with its confidence interval when the object carries one — for example `anova_test(ci = )` — otherwise as a point estimate; bounded effect sizes (eta-squared, `r`, Cliff's delta, rank-biserial) drop the leading zero while Cohen's d keeps it. For example, `get_test_label(anova_test(df, len ~ dose, effect.size = "pes", ci = 0.95), type = "text", style = "apa")` gives `F(2, 57) = 67.42, p < .001, eta2[p] = .70, 95% CI [.57, .79]`; the estimate and interval in the label are re-derived from the row's `F` and degrees of freedom at full precision, and the interval is labelled with the confidence level it was computed at. The default `style = "classic"` is unchanged.

- `effect.size = TRUE` now covers paired tests. `cohens_d()` gains an `id` argument that matches the two groups by subject (as `t_test()` already does), so `t_test(paired = TRUE, id = , effect.size = TRUE)` reports a Cohen's d computed on the same subject-matched differences as the test. A paired `wilcox_test(effect.size = TRUE)` now adds a `rank.biserial` column — the matched-pairs rank-biserial correlation `(R+ - R-) / (R+ + R-)` — computed on the paired differences the test used (matched by `id` when supplied); it carries no magnitude, since no threshold set is calibrated for it. When every paired difference is zero the matched-pairs rank-biserial is undefined (0/0): it is returned as `NA` with a warning, and the other comparisons of a pairwise call are still computed. Both previously errored.

- `t_test()`, `wilcox_test()`, `dunn_test()` and `games_howell_test()` gain an `effect.size` argument (default `FALSE`). With `effect.size = TRUE` each pairwise comparison is annotated with the effect size natural to that test, named by the metric: `cohens.d` for `t_test()` (per-pair Cohen's d), `cliff.delta` for `wilcox_test()`, `r` for `dunn_test()` (`r = Z / sqrt(N)`, the total sample size), and a Welch `cohens.d` for `games_howell_test()`; Cohen's d and Cliff's delta also get a `magnitude`. Each metric is computed consistently with the test that reports it — `pairwise_t_test(pool.sd = TRUE)` reports the common-SD (pooled-model) d that matches its pooled-SD p-value, and the Games-Howell d is oriented like its own mean difference. The default is off, so existing output is unchanged. Paired designs (including a paired t-test matched by `id` and a paired Wilcoxon) are covered too — see the entry above; `effect.size = TRUE` remains undefined only for a one-sample Wilcoxon.

- `cliff_delta()` computes Cliff's delta, a non-parametric effect size for the difference between two groups: `delta = (#{x > y} - #{x < y}) / (n1 * n2)`, ranging from -1 to 1 and unaffected by the distribution's shape (Cliff, 1993). It follows the same interface as `cohens_d()` and `wilcox_effsize()` — a formula, `ref.group`, `comparisons`, grouped data via `dplyr::group_by()`, and an optional percentile-bootstrap confidence interval with `ci = TRUE` — and returns a tibble with the columns `.y.`, `group1`, `group2`, `effsize`, `n1`, `n2` and `magnitude`. The magnitude is labelled by the Romano et al. (2006) thresholds. Cliff's delta is algebraically identical to the rank-biserial correlation, so the estimate equals `effectsize::rank_biserial()`.

- `omega_squared()` and `partial_omega_squared()` compute the classic (full) and partial omega-squared effect sizes for a between-subjects ANOVA model, alongside the existing `eta_squared()` / `partial_eta_squared()`. Omega squared is a less-biased estimate of the population effect size. Like `eta_squared()`, they take a fitted `aov` or `anova` model and return a named numeric vector, one value per model term; repeated-measures and mixed models are not supported. A negative point estimate (which arises for a term with F < 1) is floored at 0, since omega squared estimates a non-negative proportion of variance. The values match `effectsize::omega_squared(partial = FALSE)` and `partial = TRUE`, computed in base R from the Olejnik and Algina (2003) formulas.

- `tidy()` and `glance()` methods for the `rstatix_test` objects returned by the test functions (`t_test()`, `wilcox_test()`, `anova_test()`, `kruskal_test()`, and the rest). `tidy()` drops the internal `rstatix` classes and the stashed test arguments and returns the result as a plain tibble, so the object passes cleanly to tools that dispatch on `generics::tidy()` / `generics::glance()`, such as `broom`, `gtsummary` and `gt`. For every shape of `anova_test()` result — between-subjects, repeated-measures or mixed, grouped or not — `tidy()` returns the corrected ANOVA table, one row per term (per group), via `get_anova_table()`; the repeated-measures and mixed forms are lists holding the ANOVA table with Mauchly's test and the sphericity corrections, and their grouped forms carry a packed column, none of which `tidy()` would otherwise flatten. `glance()` returns a one-row summary with the test `method` and `n`, the number of comparisons or model terms. Previously the generics had no method for these objects and gave either a broken result or an error.

- `eta_squared()` and `partial_eta_squared()` gain a `ci` argument. With `ci = NULL` (the default) each still returns the bare named vector of effect sizes, unchanged. With a confidence level, e.g. `ci = 0.95`, each returns a tibble with one row per model term and the columns `Effect`, `effsize`, `conf.low` and `conf.high`. The interval is computed in base R by inverting the noncentral F distribution (Steiger, 2004) — the machinery `anova_test(ci = )` already uses — and matches `effectsize::eta_squared(ci = , alternative = "two.sided")`: `partial = FALSE` for `eta_squared()`, `partial = TRUE` for `partial_eta_squared()` (to about four decimals for the non-partial bounds of a small pseudo-F, where that function's inversion uses a looser tolerance). For a non-partial eta squared the interval is the noncentral-F interval of an equivalent pseudo-F built from the estimate, which is how the non-partial case reduces to the same inversion; for a partial eta squared that pseudo-F is the term's own F, so `partial_eta_squared(ci = )` reports the same bounds as `anova_test(effect.size = "pes", ci = )` whenever the two work from the same F per term — a one-way or balanced design, or `anova_test(type = 1)`; in an unbalanced multi-factor design `anova_test()`'s default type II F differs from the sequential F of the `aov` model, and so do the intervals. In a one-way design the two coincide ([#18](https://github.com/kassambara/rstatix/issues/18)).

- `cramer_v()` gains a `ci` argument to return a confidence interval for Cramer's V, computed in base R by inverting the noncentral chi-square distribution (Smithson, 2003; Steiger, 2004) — the same approach `anova_test(ci = )` already uses for partial eta squared, with the chi-square in place of the F distribution. With `ci = TRUE` the function returns a one-row tibble with the columns `effsize`, `conf.low` and `conf.high` — the same confidence-interval columns `anova_test(ci = )` returns — and takes a `conf.level` argument (default 0.95); the default `ci = FALSE` still returns the bare numeric value, unchanged. The interval is computed from the same chi-square statistic as the point estimate, so `correct = TRUE` shifts both. The bounds are clipped to the `[0, 1]` range of Cramer's V, and are `NA` with a warning when the statistic or its degrees of freedom are undefined (an empty row or column, or `simulate.p.value = TRUE`). Near independence — when the observed chi-square falls below the `alpha/2` quantile of its central distribution — both bounds collapse to `[0, 0]` and the point estimate lies above them; this is a property of the noncentral inversion. At the default `correct = FALSE` the interval matches `DescTools::CramerV(conf.level = )`, and `effectsize::cramers_v(adjust = FALSE, ci = , alternative = "two.sided")` away from the near-independence corner, where numerical inversions differ.

## Main changes

- **`cramer_v()` no longer applies Yates' continuity correction by default: `correct` now defaults to `FALSE`.** Cramer's V is `sqrt(chi2 / (N * (k - 1)))` computed from the Pearson chi-square statistic (Cramer, 1946). Yates' correction improves the chi-square approximation to the *null* distribution of the test statistic, so it belongs to the test — `chisq_test()`, `mcnemar_test()` and `prop_test()` keep it on by default — and not to an effect size, where it merely shrinks the statistic and biases V downward. **This changes the value returned for 2x2 tables** (the only shape Yates affects): `cramer_v(as.table(rbind(c(20, 30), c(35, 15))))` now returns `0.3015113` instead of `0.2814106`, matching the definition and the values returned by `DescTools::CramerV()` and `effectsize::cramers_v(adjust = FALSE)`. Larger tables are unaffected. Pass `correct = TRUE` to recover the previous value ([#293](https://github.com/kassambara/rstatix/issues/293)).

## Minor changes

- Function help pages and the README now link to the corresponding Datanovia tutorials.

- New documentation topic `?rstatix-references` collecting the sources of the methods `rstatix` implements — Cramer (1946), Smithson (2003), Steiger (2004), Conover (1999), Nemenyi (1963), Piepho (2004), Dunnett (1955) — and naming the packages that implement some of the same methods and offer functionality `rstatix` does not: `effectsize`, `DescTools`, `multcomp`, `multcompView` and `PMCMRplus`. Every function that documents such an agreement now names the function it agrees with, and the arguments needed to reproduce it: `conover_test()`, `friedman_conover_test()` and `friedman_nemenyi_test()` record the corresponding `PMCMRplus` function, and `anova_test(ci = )` records `effectsize::eta_squared(partial = TRUE, ci = , alternative = "two.sided")`. The agreements of `dunnett_test()` with `DescTools::DunnettTest()` and `multcomp::glht()`, and of `sign_test()` with `DescTools::SignTest()`, which the documentation already claimed, are now checked by the test suite.

- The note on `sign_test()` now records that its test code and its confidence interval for the median are adapted from `DescTools::SignTest()` and `DescTools::MedianCI()`, written by Andri Signorell, rather than only naming the package. `?rstatix-references` records the same, and separates the functions whose code is adapted from another package from the several that call one.

- The internal helpers `get_manova_table()`, `Pillai()`, `Wilks()`, `HL()` and `Roy()` are removed. They were never exported and nothing has called them since they were added in 2019. No user-visible behaviour changes.

- `cohens_d()`, `wilcox_effsize()`, `kruskal_effsize()` and `friedman_effsize()` gain `boot.parallel` and `boot.ncpus` arguments to compute the bootstrap confidence interval (`ci = TRUE`) in parallel, for example `cohens_d(df, len ~ supp, ci = TRUE, boot.parallel = "multicore", boot.ncpus = 4)`. They default to `getOption("boot.parallel", "no")` and `getOption("boot.ncpus", 1L)`, so the bootstrap stays serial by default, the corresponding global options keep working, and all existing results are unchanged. The parallel settings are now passed to `boot::boot()`, which performs the resampling, rather than to `boot::boot.ci()`, which has no such argument and silently ignored them. Based on the contribution by @HannesOberreiter ([#289](https://github.com/kassambara/rstatix/pull/289), [#87](https://github.com/kassambara/rstatix/pull/87)).

## Bug fixes

- `eta_squared()` and `partial_eta_squared()` now exclude the `(Intercept)` row that a `car::Anova()` type 3 table carries. That row's sum of squares belongs to neither the effect decomposition nor the residual, so counting it inflated the eta-squared denominator — with `cyl` and `am` as factors, for `car::Anova(lm(mpg ~ cyl * am, data = mtcars, contrasts = list(cyl = "contr.sum", am = "contr.sum")), type = 3)`, `eta_squared()` returned 0.042 for `cyl` where the correct value is 0.582 — and `partial_eta_squared()` reported a meaningless `(Intercept)` effect row. Both now match `effectsize::eta_squared(partial = FALSE)` / `(partial = TRUE)` on such tables; `aov` and `stats::anova()` inputs, which carry no intercept row, are unchanged. The new `omega_squared()` and `partial_omega_squared()` apply the same exclusion.

- `R CMD check` no longer fails when the packages in `Suggests` are absent, the configuration CRAN checks with `_R_CHECK_DEPENDS_ONLY_=true`. `dunnett_test()` and `emmeans_test()` stop without `emmeans`, and `wilcox_effsize()` stops without `coin`; their examples now run only when the package is installed, and the two test blocks that reached them without a guard now skip. The examples of `dunnett_test()` and `emmeans_test()` are wrapped in `requireNamespace()` rather than `\donttest{}`, which does not help: `R CMD check --as-cran` runs those blocks as well. A `noSuggests` job is added to the check matrix so the configuration is exercised ([#296](https://github.com/kassambara/rstatix/issues/296)).

- The bootstrap confidence interval (`ci = TRUE`) of `cohens_d()`, `wilcox_effsize()`, `kruskal_effsize()` and `friedman_effsize()` no longer breaks when the bootstrap replicates are **degenerate** — all identical (a perfect effect size, e.g. Kendall's *W* = 1) or all missing (constant data). Such an interval is undefined: `conf.low` and `conf.high` are now returned as `NA` with a warning, and the remaining groups or comparisons are still computed. Previously an ungrouped call silently returned empty **list-columns** in place of the bounds, a grouped call failed with `Can't combine ... <list> and ... <double>`, and constant data errored with `missing value where TRUE/FALSE needed`. An interval type that cannot be built (for instance `ci.type = "stud"`, which needs bootstrap variances) is likewise returned as `NA` with a warning instead of a malformed column. Well-behaved bootstrap intervals are unchanged ([#290](https://github.com/kassambara/rstatix/issues/290)).


# rstatix 1.0.0

## New features

- `anova_test()` gains a `ci` argument to add a **confidence interval for partial eta squared**. With `effect.size = "pes"` and e.g. `ci = 0.95`, two columns `conf.low`/`conf.high` are returned, computed in base R from the noncentral F distribution (Steiger, 2004) — no new dependency. The intervals match `effectsize::eta_squared(partial = TRUE, ci = , alternative = "two.sided")`. Works for one-way, factorial, repeated-measures and mixed designs (using the uncorrected degrees of freedom, consistent with the effect-size point estimate). No interval is provided for generalized eta squared (`"ges"`, the default), which has no standard closed-form interval. The default `ci = NULL` leaves the output unchanged ([#18](https://github.com/kassambara/rstatix/issues/18)).
- `t_test()` and `wilcox_test()` gain an `error.as.na` argument: when `TRUE`, a comparison that cannot be computed (a group with fewer than two observations, or essentially constant data) returns an `NA` result row with a warning naming the comparison, instead of stopping with an error — so the remaining comparisons (or groups, in a grouped analysis) are still computed. The default `error.as.na = FALSE` keeps the previous behavior (the comparison errors) ([#208](https://github.com/kassambara/rstatix/issues/208), [#158](https://github.com/kassambara/rstatix/issues/158)).
- `t_test()` and `wilcox_test()` gain an `id` argument for **paired** tests: it names the subject/sample identifier so the two compared groups are matched by subject (instead of by row order), using only subjects present in both groups (complete pairs). For more than two groups the matching is done independently per pairwise comparison, so comparisons can be based on different numbers of pairs (per-comparison pairwise deletion, like SPSS). This makes paired tests work when observations are missing or the groups have unequal sizes, and avoids silent mis-pairing when rows are not in subject order. The default (`id = NULL`) is unchanged (groups paired in row order) ([#136](https://github.com/kassambara/rstatix/issues/136), [#175](https://github.com/kassambara/rstatix/issues/175), [#192](https://github.com/kassambara/rstatix/issues/192)).
- `cor_test()` now returns the degrees of freedom (`df`) for the Pearson method (e.g. to report `r(df) = …, p`); the column sits next to `statistic`. Spearman/Kendall (which have no df) and `cor_mat()`/`cor_pmat()` are unchanged ([#107](https://github.com/kassambara/rstatix/issues/107)).
- `get_summary_stats()` can now report **skewness** and **kurtosis** (bias-corrected, type-2 estimator, matching `e1071`'s `type = 2`). They are opt-in via `show`, e.g. `get_summary_stats(data, x, show = c("mean", "sd", "skewness", "kurtosis"))`, and are not added to any default `type`, so existing output is unchanged ([#99](https://github.com/kassambara/rstatix/issues/99)).
- `get_summary_stats()` gains a `digits` argument to control the number of decimal places (default 3); useful when summarizing very small values that would otherwise round to 0 ([#145](https://github.com/kassambara/rstatix/issues/145), [#186](https://github.com/kassambara/rstatix/issues/186), [#218](https://github.com/kassambara/rstatix/issues/218)).
- `pairwise_mcnemar_test()` now returns the McNemar chi-squared `statistic` and `df` columns for `type = "mcnemar"` (the default), as the documentation already described; they were previously dropped. The exact-binomial path (`type = "exact"`), `mcnemar_test()`, and the `p`/`p.adj` values are unchanged ([#122](https://github.com/kassambara/rstatix/issues/122)).
- `dunn_test()` gains a `ref.group` argument to compare each group only to a reference (control) level. When specified, the multiple-comparison adjustment is computed over only the `k - 1` comparisons against the reference (matching e.g. GraphPad Prism), which is not equivalent to filtering the full pairwise result afterwards. The default `ref.group = NULL` performs all pairwise comparisons exactly as before ([#101](https://github.com/kassambara/rstatix/issues/101)).
- `chisq_test()` now accepts a pipe-friendly data-frame interface for the test of independence between two categorical variables: `data %>% chisq_test(var1, var2)` (positional) or `data %>% chisq_test(vars = c("var1", "var2"))`. The contingency table is built internally. The existing table/matrix/vector interfaces (including passing a data frame that already is a contingency table) are unchanged ([#43](https://github.com/kassambara/rstatix/issues/43)).
- `wilcox_effsize()` gains a `detailed` argument: when `detailed = TRUE`, the output additionally includes the `Z` `statistic` (used to compute `r = Z/sqrt(N)`), the p-value and the test method, so the effect size and the underlying Z are reported in a single data frame. The default (`detailed = FALSE`) output is unchanged ([#122](https://github.com/kassambara/rstatix/issues/122)).
- New function `dunnett_test()` for Dunnett's many-to-one comparisons: each treatment group is compared against a single control (`ref.group`), with the family-wise error rate controlled over only the `k - 1` comparisons using the exact multivariate-t distribution. Results match `DescTools::DunnettTest()` / `multcomp::glht()`. Built on `emmeans` (Suggests) ([#129](https://github.com/kassambara/rstatix/issues/129)).
- New function `ks_test()` — a pipe-friendly wrapper around `stats::ks.test()` for the two-sample Kolmogorov-Smirnov test, with automatic pairwise comparisons (and `ref.group`) for grouping variables with more than two levels ([#92](https://github.com/kassambara/rstatix/issues/92), [#168](https://github.com/kassambara/rstatix/issues/168)).
- New function `conover_test()` for Conover's (Conover-Iman) all-pairs rank comparison test, a post-hoc procedure following a significant Kruskal-Wallis test. It mirrors `dunn_test()` (including `ref.group` and grouped-data support) but uses the pooled within-group rank variance and a *t*-distribution, making it generally more powerful. Implemented in base R (no new dependency); results match `PMCMRplus::kwAllPairsConoverTest()` ([#222](https://github.com/kassambara/rstatix/issues/222), [#17](https://github.com/kassambara/rstatix/issues/17)).
- New function `friedman_conover_test()` for Conover's all-pairs comparisons (also known as the Durbin-Conover test), a post-hoc procedure following a significant Friedman rank sum test (the repeated-measures analogue of Conover's Kruskal-Wallis post-hoc). It supports `ref.group` and grouped data. Implemented in base R (no new dependency); results match `PMCMRplus::frdAllPairsConoverTest()` ([#8](https://github.com/kassambara/rstatix/issues/8)).
- New function `friedman_nemenyi_test()` for the Nemenyi all-pairs post-hoc test following a significant Friedman rank sum test (the rank-based, repeated-measures analogue of Tukey's HSD). The statistic is referred to the studentized range distribution, which already accounts for the multiplicity of the comparisons (so the reported `p.adj` needs no further adjustment). Implemented in base R (no new dependency); results match `PMCMRplus::frdAllPairsNemenyiTest()` ([#141](https://github.com/kassambara/rstatix/issues/141)).
- New function `add_cld()` adds the **compact letter display** to an all-pairwise comparison result (e.g. from `tukey_hsd()`, `dunn_test()`, `games_howell_test()`, `conover_test()` or a pairwise `t_test()`/`wilcox_test()`): groups that do not share a letter are significantly different — convenient for annotating plots. The letters are computed in base R via the insert-and-absorb algorithm (Piepho, 2004), matching `multcompView::multcompLetters()` but without adding a dependency. Works on grouped tests (one display per group) ([#110](https://github.com/kassambara/rstatix/issues/110)).
- New function `fligner_test()` — a pipe-friendly wrapper around `stats::fligner.test()` for the Fligner-Killeen test, a non-parametric (rank-based) test of the homogeneity of group variances and a robust alternative to `levene_test()`. Supports grouped data and reports the complete-case `n` ([#179](https://github.com/kassambara/rstatix/issues/179)).

## Main changes

- **P-values are no longer rounded to 3 significant figures before being returned.** Test functions (`t_test()`, `wilcox_test()`, `kruskal_test()`, `cor_test()`, `tukey_hsd()`, `games_howell_test()`, `welch_anova_test()`, the `pairwise_*`/`*_test` proportion/chi-square/Fisher/binomial/McNemar families, etc.) now return full-precision `p` and `p.adj`, consistent with `dunn_test()` which already did. Adjusted p-values are now computed from the full-precision p-values, so re-running `adjust_pvalue()` (or `p.adjust()`) downstream is exact. **This changes the stored/printed `p`/`p.adj` (more digits), and pairwise `p.adj` values can shift slightly because adjustment is no longer applied to pre-rounded inputs.** To round for display, use `p_format()` / `p_round()` or `options(pillar.sigfig=)`. Test statistics, effect sizes, the correlation coefficient, and the omnibus ANOVA tests (`anova_test()`, `welch_anova_test()`, whose summary p stays at 3 significant figures) are unaffected ([#108](https://github.com/kassambara/rstatix/issues/108), [#135](https://github.com/kassambara/rstatix/issues/135), [#219](https://github.com/kassambara/rstatix/issues/219)).
- The `method` column of detailed test results (`detailed = TRUE`) now reports the specific test variant instead of a generic label: `t_test()` gives "Welch t-test" / "T-test" / "Paired t-test" / "One-sample t-test", and `wilcox_test()` gives "Wilcoxon rank sum test" / "Wilcoxon signed rank test" ([#124](https://github.com/kassambara/rstatix/issues/124)). Non-detailed output and the p-values/statistics are unchanged.

## Minor changes

- `freq_table()` now supports **grouped data**: `data %>% group_by(g) %>% freq_table(x)` computes the frequency table of `x` within each group (the grouping columns are kept and the proportions sum to 100% within each group), returning a single tidy data frame. Previously a grouped call errored. Ungrouped behavior is unchanged. Based on the contribution by @jakub-jedrusiak ([#191](https://github.com/kassambara/rstatix/pull/191)).
- Documented how to use `emmeans_test()`'s `model` argument for two common cases: (i) computing estimated marginal means **averaged over another factor** in a factorial design (fit `lm(y ~ a * b)` and compare `a` via `formula = y ~ a`, `model = `), and (ii) **repeated-measures / mixed designs** (pass a within-subject model such as `aov(... + Error())` or `nlme::lme()`). Both already worked via `model = ` — this clarifies the parameter and adds examples ([#139](https://github.com/kassambara/rstatix/issues/139), [#131](https://github.com/kassambara/rstatix/issues/131)).
- Documented the columns added by `add_y_position()` / `add_x_position()` / `add_xy_position()`, and clarified that the `groups` column is a **reserved internal column** (used to position dodged grouped comparisons) that is overwritten — users should not rely on it or use `groups` as one of their own column names in the test object. A future major version may rename it to a dotted, less collision-prone name ([#16](https://github.com/kassambara/rstatix/issues/16)).
- Documented that `anova_test()`'s **default sums-of-squares type can differ between its two interfaces** on unbalanced designs: the `formula` interface defaults to type II for between-subjects designs, while the `dv=`/`between=` interface uses type III for unbalanced between-subjects designs with more than one factor (both use type III for repeated measures). For balanced designs the types coincide; for unbalanced designs, pass `type=` explicitly for reproducible, interface-independent results. A `Note` and example were added to `?anova_test`. No change to computed results ([#190](https://github.com/kassambara/rstatix/issues/190)).
- New documentation topic `?\`rstatix-programming\`` showing how to use `rstatix` programmatically (variable names held in strings, wrapper functions). The selection interface (e.g. `cor_test()`, `get_summary_stats()`) supports tidy evaluation — `!!`/`!!!`, embracing with `{{ }}`, `vars=`, and `all_of()`/`any_of()` — and the formula tests (e.g. `t_test()`, `anova_test()`) take a formula built with `reformulate()` / `as.formula(paste())`. These patterns already work; they are now documented and cross-referenced from the relevant help pages ([#142](https://github.com/kassambara/rstatix/issues/142)).
- `cor_test()` now errors with a clear message when a `weights` argument is supplied, instead of silently returning the **unweighted** correlation (it wraps `stats::cor.test()`, which has no `weights` argument). The message points to base R `stats::cov.wt(..., cor = TRUE)` for a weighted Pearson correlation. Unweighted calls are unaffected ([#47](https://github.com/kassambara/rstatix/issues/47)).
- The "reference group not present" error (see below) now carries the S3 condition class `rstatix_missing_ref_group`, so downstream callers (e.g. `ggpubr::geom_pwc()`, which skips ref-less grouped subsets) can detect it reliably by class — via `rlang::cnd_inherits()` to also catch it through the grouped (`doo()`/purrr) wrapping — instead of matching the translatable message text ([#153](https://github.com/kassambara/rstatix/issues/153)).
- `t_test()` / `wilcox_test()` (and the other tests that accept `ref.group`) now give a clear, actionable error when the specified `ref.group` is not present in the data, instead of a cryptic `'<ref>' must be an existing level` from `relevel()`. This most often happens with grouped data where some group does not contain the reference level; the message lists the available levels and shows the `group_by() %>% filter(any(... == ref.group))` idiom to keep only the groups that have it. Valid `ref.group` values (including `ref.group = "all"`) are unaffected ([#153](https://github.com/kassambara/rstatix/issues/153)).
- Documented the **sign convention** of `t_test()` / `wilcox_test()` when a `ref.group` is used: the reference is taken as `group1`, so `estimate = mean(group1) - mean(group2) = mean(ref.group) - mean(other)` (the base R `t.test()`/`wilcox.test()` convention) and a positive `statistic`/`estimate` means the value is higher in the reference group. The note also shows how to flip the sign if you prefer a positive sign to mean "higher in the non-reference group". Documentation only — computed values are unchanged ([#153](https://github.com/kassambara/rstatix/issues/153)).
- `add_xy_position()` / `add_x_position()` with `scales = "free"` now compute the bracket x positions (`xmin`/`xmax`) **per facet**, so they align correctly when a faceted plot (`facet_*(scales = "free")`) shows a different set of x-axis levels in each panel. Previously the global x positions were used, placing brackets off-panel in facets that didn't start from the first level. `scales = "fixed"` (default) and `"free_y"` are unchanged ([#203](https://github.com/kassambara/rstatix/issues/203)).
- `add_y_position()` / `add_xy_position()` now handle repeated-measures test results whose stashed formula has the `outcome ~ within | subject` form (e.g. `friedman_conover_test()`): the `| subject` block is dropped when locating the x/grouping variable, so the brackets position correctly. Ordinary `outcome ~ group` results are unaffected ([#8](https://github.com/kassambara/rstatix/issues/8)).
- `anova_test()` now gives clear, actionable error messages for two common repeated-measures data problems instead of cryptic ones: when the data has more than one observation per subject per within-subject cell ("duplicated cells"), and when no subject has a complete set of within-cell observations (previously a `0 (non-NA) cases` error). Valid designs are unaffected ([#216](https://github.com/kassambara/rstatix/issues/216), [#146](https://github.com/kassambara/rstatix/issues/146), [#116](https://github.com/kassambara/rstatix/issues/116), [#134](https://github.com/kassambara/rstatix/issues/134), [#102](https://github.com/kassambara/rstatix/issues/102)).
- Updated the CRAN checks badge in the README to the current `badges.cranchecks.info` endpoint (the old `cranchecks.info` badge no longer resolves) ([#174](https://github.com/kassambara/rstatix/issues/174)).
- Clarified the `?anova_test` documentation about contrasts: the formula / `dv` + `between`/`within` interface fits the model internally with `contr.sum` (matching SPSS / type-III), whereas a pre-fitted `lm()`/`aov()` keeps its own contrasts (R's default `contr.treatment` unless set otherwise) ([#225](https://github.com/kassambara/rstatix/issues/225)).
- Clarified the `?wilcox_effsize` documentation of the sample size `N` used in `r = Z/sqrt(N)`. For the one-sample and paired tests, `N` is the **number of pairs** (difference scores), because the paired test reduces to a one-sample signed-rank test on the differences; this matches the default of `rcompanion::wilcoxonPairedR()`. The alternative convention `N = 2 x pairs` (Field, 2012; Tomczak & Tomczak, 2014) is now documented, with how to obtain it. Documentation only — computed effect sizes are unchanged ([#213](https://github.com/kassambara/rstatix/issues/213)).
- Removed all internal **tidyselect** deprecation warnings (`Use of .data in tidyselect expressions...`, `Using an external vector in selections...`, and `mutate_at()`/`pull()` external-vector usage) across the package by selecting columns with `all_of()`/`any_of()`. This is an internal change only — results are unchanged — but it future-proofs `rstatix` against upcoming `tidyselect` versions and cleans up the test output ([#202](https://github.com/kassambara/rstatix/issues/202)).

## Bug fixes

- `kruskal_test()` (and therefore `kruskal_effsize()`) now reports the number of observations actually used by the test as `n`, rather than `nrow(data)`. When the outcome or group contained missing values, `kruskal.test()` dropped them but the reported `n` was the inflated raw row count; because `kruskal_effsize()` (eta²[H]) uses `n` in its denominator, the effect size could be wrong too. `n` is now the complete-case count (matching the test's own NA handling); the test statistic and p-value are unchanged, and data without NAs report the same `n` as before ([#224](https://github.com/kassambara/rstatix/issues/224)).
- Grouped repeated-measures `anova_test()` followed by `get_anova_table()` no longer errors (`Can't combine <data.frame> and <list>`) when the per-group results have different shapes — for example when one group needs a sphericity correction (so its result carries the Mauchly/sphericity-correction tables) while another does not. The internal `doo()` helper now decides whether to unnest based on all groups rather than only the first, keeping heterogeneous results in a list-column so `get_anova_table()` can extract each group. Homogeneous grouped results (the usual case) are unchanged ([#83](https://github.com/kassambara/rstatix/issues/83)).
- `wilcox_test()` no longer hides the warning when the underlying `wilcox.test()` **silently lowers the confidence interval's confidence level** (e.g. to 60% instead of the requested 95%) because it cannot be achieved with tied or zero data. Previously this was suppressed, so the returned interval looked like a 95% CI and could contradict the p-value (e.g. `p > 0.05` while the CI excludes 0). A clear warning is now emitted in that case (only when `detailed = TRUE`, i.e. when a CI is requested). The returned values are unchanged; `t_test()` and clean-data calls are unaffected ([#127](https://github.com/kassambara/rstatix/issues/127)).
- `games_howell_test()` no longer crashes (`` `df` must be size 15 or 1, not 12 ``) when groups have **zero variance** (constant values) or **undefined variance** (a single observation). The Welch correction is undefined for the affected pairs (degrees of freedom `0/0`, or `NA` variance), so those comparisons are now returned as `NA` with a warning while all other comparisons are computed as usual. Previously such data either errored or silently returned recycled/incorrect values. Results for data without zero/undefined-variance groups are unchanged ([#183](https://github.com/kassambara/rstatix/issues/183)).
- `add_xy_position()`/`add_y_position()` now keep significance brackets evenly spaced after the test results have been **filtered** (e.g. to keep only significant comparisons). Previously the y positions were computed for the full comparison set and then joined onto the filtered rows, producing uneven spacing; they are now computed for exactly the comparisons present. Unfiltered results, `ref.group = "all"`, one-sample and grouped tests are unchanged ([#197](https://github.com/kassambara/rstatix/issues/197)).

- `cor_test()` (and `cor_mat()`) no longer emit the tidyselect "Using an external vector in selections was deprecated" warning when `vars`/`vars2` are passed as character vectors (e.g. `cor_test(data, vars = my_vars)`); the columns are now selected via `all_of()`. Bare names and tidyselect helpers are unaffected ([#202](https://github.com/kassambara/rstatix/issues/202)).
- `p_format()` no longer strips a trailing `0` from the exponent of scientific-notation p-values (e.g. `p_format(5.1e-10)` returned `"5.1e-1"`; it now correctly returns `"5.1e-10"`). Ordinary decimal formatting, including `trailing.zero` padding, is unchanged ([#112](https://github.com/kassambara/rstatix/issues/112)).
- `p_mark_significant()` no longer produces `"e-NA"` (with a coercion warning) when given a scientific-notation p-value string such as the output of `p_format()` on `1e-4`; it now correctly returns e.g. `"1e-04****"` ([#148](https://github.com/kassambara/rstatix/issues/148)).
- `get_y_position()`/`add_y_position()` now space significance brackets by exactly `step.increase`. Previously the gap between consecutive brackets was `step.increase * n/(n-1)` (wider than requested) for `n >= 2` comparisons. **This changes the computed `y.position` values for plots with 3 or more groups** (brackets are slightly closer together); single-comparison (two-group) plots and `ref.group = "all"` are unchanged ([#201](https://github.com/kassambara/rstatix/issues/201)).
- `get_test_label()` now includes the sample size `n` for ANOVA results (e.g. `Anova, F(1,58) = 105.06, p = <0.0001, eta2[g] = 0.644, n = 60`), consistent with the other tests; it was previously dropped because the slicing step stripped the attributes `get_n()` needs ([#150](https://github.com/kassambara/rstatix/issues/150)).
- The comparison tests (`t_test()`, `wilcox_test()`, `dunn_test()`, `emmeans_test()`, etc.) now drop unused levels of the grouping factor, so a filtered factor that still carries empty levels no longer triggers "not enough observations" errors — matching `stats::t.test()` ([#133](https://github.com/kassambara/rstatix/issues/133)).
- `wilcox_test()` and `pairwise_wilcox_test()` no longer error on degenerate (all-tied / constant) data. The location confidence interval (which can fail to compute on such data) is now requested only when `detailed = TRUE`, so the default call returns gracefully; `statistic`/`p` are unchanged ([#79](https://github.com/kassambara/rstatix/issues/79), [#167](https://github.com/kassambara/rstatix/issues/167)).
- `anova_test()` results (grouped and ungrouped) and `get_anova_table()` output are now compatible with `dplyr` verbs again (e.g. `filter()`, `mutate()`, `add_xy_position()`): the class order is corrected so `rstatix_test` precedes `data.frame`, which recent `vctrs`/`dplyr` require ([#106](https://github.com/kassambara/rstatix/issues/106), [#111](https://github.com/kassambara/rstatix/issues/111)).
- `cohens_d()` now honours the `mu` argument for two-sample tests (independent and paired): `mu` is the hypothesized mean difference and is subtracted before standardizing, consistent with the one-sample case. Previously `mu` was silently ignored for two-sample tests. Calls using the default `mu = 0` are unchanged ([#200](https://github.com/kassambara/rstatix/issues/200)).
- `emmeans_test()` no longer fails with "Nonconforming number of contrast coefficients" when the `covariate` is a numeric variable with only two distinct values (e.g. a 0/1 indicator). The covariate is now correctly averaged over (ANCOVA) instead of being kept as a grid factor ([#206](https://github.com/kassambara/rstatix/issues/206), [#86](https://github.com/kassambara/rstatix/issues/86)).
- `kruskal_effsize()` now clamps the eta-squared effect size to its valid `[0, 1]` range, so a near-null effect no longer returns a negative value (the formula could yield a small negative for a tiny `H`); consequently it is correctly labelled "small" rather than "large" (the magnitude had used the absolute value) ([#217](https://github.com/kassambara/rstatix/issues/217)).
- `anova_test()` / `anova_summary()` now return **both** effect sizes when `effect.size = c("pes", "ges")` is requested; previously only partial eta squared was kept ([#180](https://github.com/kassambara/rstatix/issues/180)).
- `tukey_hsd()` now respects `conf.level` (and other `TukeyHSD()` arguments) for the ungrouped data-frame interface; previously `...` was dropped so the confidence interval was always 95% ([#188](https://github.com/kassambara/rstatix/issues/188)).
- `pairwise_binom_test_against_p()` no longer errors on an unnamed `x`; groups are auto-labelled `grp1`, `grp2`, … (named/table input is unchanged) ([#44](https://github.com/kassambara/rstatix/issues/44)).
- Fixed a missing space in the `anova_test()` error message for single-level factors (now reads "Variable x has only one level") ([#137](https://github.com/kassambara/rstatix/issues/137)).
- `anova_test()` results now carry `rstatix_test` as the **first** class, with the specific test class (`anova_test` / `grouped_anova_test`) in second position — matching every other rstatix test. The [#106](https://github.com/kassambara/rstatix/issues/106) dplyr/vctrs invariant (`rstatix_test` before `data.frame`) is preserved, but the earlier `c("anova_test", "rstatix_test", "data.frame")` order had shifted the specific class out of position 2, breaking reverse dependencies that dispatch on `class(x)[2]` (e.g. GimmeMyStats, GimmeMyPlot). `inherits()`, `print()`/`plot()` dispatch, dplyr verbs and ggpubr are unaffected.


# rstatix 0.7.3


## Bug fixes

- Fixed CRAN check errors related to R-devel changes in Wilcoxon tests. Updated tests to accept both legacy and R-devel p-values when exact conditional two-sample inference with ties is used (R-devel r88748). Tests now use flexible assertions to ensure compatibility across R versions ([#220](https://github.com/kassambara/rstatix/issues/220)).
- Fixed documentation formatting errors: removed trailing spaces in `\item{}` syntax in `box_m.Rd` and corrected `{v}` to `\code{v}` in `wilcox_test.Rd` ([#220](https://github.com/kassambara/rstatix/issues/220)).
- Fixed roxygen2 warning about `sign.test()` internal function by adding `@noRd` tag.


# rstatix 0.7.2

## Minor changes

- Required `tidyselect` versions is `>= 1.2.0`

## Bug fixes
   
- `emmeans_test()`: restoring grouping variable class (`factor`) in the final results `emmeans_test()` (#169)
- Fix warning in `emmeans_test()`:  "Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0."
  
   
# rstatix 0.7.1


## Minor changes

- `cor_plot()` now accepts additional arguments to pass to  corrplot() (#66)
- suppressMessages() used to suppress this message ("Coefficient covariances computed by hccm()") generated by `car::Anova()`. 
- `get_comparisons()` now drops unused levels before creating possible comparisons (#67)
- Now, the function `get_summary_stats()` keeps the order of columns specified by the user (#46).
- internal `two_sample_test()` now counts group sizes (`n1` and `n2`) by the number of non-`NA` values [#104](https://github.com/kassambara/rstatix/issues/104)

## Bug fixes

- Name collisions bug fixes in the `shapiro_test()` function. Shapiro_test() throws an error if the input data contains column names "value" or "variable". This is fixed now (#52).
- Bug fixed in the `cor_test()` function, where there was a tidy evaluation conflict when the input data contains "x" and "y" as column names (#68).
- The `dunn_test()` documentation is updated to describe the discrepancy between the default behavior of the `rstatix::dunn_test()` compared to other packages (`dunn.test` and `jamovi`). The default of the rstatix::dunn_test() function is to perform a two-sided Dunn test like the well known commercial softwares, such as SPSS and GraphPad. This is not the case for some other R packages (dunn.test and jamovi), where the default is to perform one-sided test (#50). 
- Now, the function `get_summary_stats()` handles the user defined probabilities for grouped data (#78)


# rstatix 0.7.0

## New features

- New function to extract information from rstatix statistical tests:
      - `get_n()` to extract sample count (n) from statistical test results.
      - `get_description` to extract stat test description or name
      - `remove_ns()` to remove non-significant rows.

## Major changes

- Rewriting `add_x_position()` to better support different situations (#73).
- Now, the output of the function `dunn_test()` include `estimate1` and `estimate2` when the argument `detailed = TRUE` is specified. The `estimate1` and `estimate2` values represent the mean rank values of the two groups being compared, respectively (#59).

## Minor changes

- `cor_spread()` doc updated, error is explicitly shown if the input data doesn't contain the columns "var1", "var2" and "cor" (#95)
- Maintenance updates of the functions `emmeans_test()` and `levene_test()` to adapt to broom release 0.7.4 (#89)
- The documentation of the function `anova_test()` is updated to explain the internal contrast setting (#74).
- Now, `p_mark_significance()` works when all p-values are NA. Empty character ("") is returned for NA (#64).
- Classes (`rstatix` and `grouped_anova_test`) added to grouped ANOVA test (#61)
- New argument `scales` added in the function `get_y_position()`. If the specified value is "free" or "free_y", then the step increase of y positions will be calculated by plot panels. Note that, using "free" or "free_y" gives the same result. A global step increase is computed when scales = "fixed" (#56).

## Bug fixes
   
- The function `anova_test()` computes now repeated measures ANOVA without error when unused columns are present in the input data frame (#55)

# rstatix 0.6.0
   
## Minor changes
   
- Adapted to upcoming broom v0.7.0 release (#49)
- New argument `stack` added in `get_y_position()` to compute p-values y position for stacked bar plots ([#48](https://github.com/kassambara/rstatix/issues/48)).
- `wilcox_test()`: Now, if `detailed = TRUE`,  an estimate of the location parameter (Only present if argument detailed = TRUE). This corresponds to the pseudomedian (for one-sample case) or to the difference of the location parameter (for two-samples case) ([#45](https://github.com/kassambara/rstatix/issues/45)).

## Bug fixes
   
- `anova_test()` function: Changing R default contrast setting (`contr.treatment`) into orthogonal contrasts (`contr.sum`) to have comparable results to SPSS when users define the model using formula (@benediktclaus, [#40](https://github.com/kassambara/rstatix/issues/40)).
- Now, the option `type = "quantile"` of `get_summary_stats()` works properly (@Boyoron, [#39](https://github.com/kassambara/rstatix/issues/39)).


# rstatix 0.5.0
  
## New features
   
- New functions added for easy data frame manipulation. These functions are internally used in the `rstatix` and the `ggpubr` package and makes it easy to program with tidyverse packages using non standard evaluation.
      - df_select
      - df_arrange
      - df_group_by
      - df_nest_by
      - df_split_by
      - df_unite
      - df_get_var_names
      - df_label_both
      - df_label_value

## Minor changes

- Now, in `freq_table()` the option `na.rm` removes only missing values in the variables used to create the frequency table (@JuhlinF, [#25](https://github.com/kassambara/rstatix/issues/25)).
- Missing values are now correctly handled in `anova_test()` (@benediktclaus, [#31](https://github.com/kassambara/rstatix/issues/31))
- Maintenance for adapting to the future dplyr 1.0.0 version [#32](https://github.com/kassambara/rstatix/issues/32)
  
## Bug fixes
  
- An informative message is now displayed when users try to apply Hedge's correction when computing the Cohen's D for one sample test (@GegznaV, [#36](https://github.com/kassambara/rstatix/issues/36)).
- Bug fixes in the `games_howell_test()` function : the t-statistic is now calculated using the **absolute** mean difference between groups (@GegznaV, [#37](https://github.com/kassambara/rstatix/issues/37)).
- x position is now correctly computed when when making custom comparisons (@barrel0luck, [#28](https://github.com/kassambara/rstatix/issues/28)).
   
   
# rstatix 0.4.0

## New features
   
- The `cohens_d()` function now supports Hedge's correction. New argument `hedge.correction` added . logical indicating whether apply the Hedges correction by multiplying the usual value of Cohen's d by `(N-3)/(N-2.25)` (for unpaired t-test) and by `(n1-2)/(n1-1.25)` for paired t-test; where N is the total size of the two groups being compared (N = n1 + n2) (@IndrajeetPatil, [#9](https://github.com/kassambara/rstatix/issues/9)).
  
## Minor changes
  
- Now, the function `cohens_d()` outputs values with directionality. The absolute value is no longer returned. It can now be positive or negative depending on the data (@narunpat, [#9](https://github.com/kassambara/rstatix/issues/13)).

## Bug fixes
  
- The value of `mu` is now considered when calculating `cohens_d()` for one sample t-test (@mllewis, [#22](https://github.com/kassambara/rstatix/issues/22)).
- The function `tukey_hsd()` now handles situation where minus `-` symbols are present in factor levels (@IndrajeetPatil, [#19](https://github.com/kassambara/rstatix/issues/19)).
  
# rstatix 0.3.1

## Minor changes

- tidyr > 1.0.0 now required
- know, `identify_outliers` returns a basic data frame instead of tibble when nrow = 0 (for nice printing)
- new argument `detailed` added in `dunn_test()`. If TRUE, then estimate and method columns are shown in the results.



# rstatix 0.3.0

## New features
   
- `prop_test()`, `pairwise_prop_test()` and `row_wise_prop_test()`. Performs one-sample and two-samples z-test of proportions. Wrappers around the R base function `prop.test()` but have the advantage of performing pairwise and row-wise z-test of two proportions, the post-hoc tests following a significant chi-square test of homogeneity for 2xc and rx2 contingency tables. 
- `fisher_test()`, `pairwise_fisher_test()` and `row_wise_fisher_test()`: Fisher's exact test for count data. Wrappers around the R base function `fisher.test()` but have the advantage of performing pairwise and row-wise fisher tests, the post-hoc tests following a significant chi-square test of homogeneity for 2xc and rx2 contingency tables. 
- `chisq_test()`, `pairwise_chisq_gof_test()`, `pairwise_chisq_test_against_p()` : Chi-square test for count data.
- `binom_test()`, `pairwise_binom_test()`, `pairwise_binom_test_against_p()` and `multinom_test()`: performs exact binomial and multinomial tests. Alternative to the chi-square test of goodness-of-fit-test when the sample.
- `counts_to_cases()`: converts a contingency table or a data frame of counts into a data frame of individual observations.
- New functions `mcnemar_test()` and `cochran_qtest()` for comparing two ore more related proportions.
- `prop_trend_test()`: Performs chi-squared test for trend in proportion. This test is also known as Cochran-Armitage trend test.


## Minor changes

- Now `get_test_label()` and `get_pwc_label()` return expression by default
- Unit testing and spelling check added
- Code rewritten to adapt tidyr 1.0.0


# rstatix 0.2.0

     
## Minor changes 
   
- `get_anova_table()` supports now an object of class `grouped_anova_test`
- ANOVA table is now correctly returned when `correction = "none"` for repeated measures ANOVA
- `NAs` are now automatically removed before quantile computation for identifying outliers (@IndrajeetPatil, [#10](https://github.com/kassambara/rstatix/issues/10)).
- Unquoted factor variable name is now supported in factor manipulation functions: `set_ref_level()`, `reorder_levels()` and `make_valid_levels()`
- New argument `model` added in the function `emmeans_test()`
- Adapting to tidyr v1.0.0 (@jennybc, [#6](https://github.com/kassambara/rstatix/issues/6))
   
  
## New features
  
- New function `welch_anova_test()`: Welch one-Way ANOVA test. A wrapper around the base function `stats::oneway.test()`. This is is an alternative to the standard one-way ANOVA in the situation where the homogeneity of variance assumption is violated.
- New function `friedman_effsize()`, computes the effect size of Friedman test using the Kendall's W value.
- New function `friedman_test()`, provides a pipe-friendly framework to perform a Friedman rank sum test, which is the non-parametric alternative to the one-way repeated measures ANOVA test.
- New function `games_howell_test()`: Performs Games-Howell test, which is used to compare all possible combinations of group differences when the assumption of homogeneity of variances is violated.
- New function `kruskal_effsize()` for computing effect size for Kruskal-Wallis test.
- New functions added to round and format p-values: `p_round(), p_format(), p_mark_significant()`.
- New function `wilcox_effsize()` added for computing effect size (r) for wilcoxon test.
- New function `get_anova_table()` added to extract ANOVA table from `anova_test()` results. Can apply sphericity correction automatically in the case of within-subject (repeated measures) designs.
- New functions added to extract information from statistical tests: `get_anova_label()`
- New function `emmeans_test()` added for pairwise comparisons of estimated marginal means.
   
   
## Minor changes
  
- the unnecessary column `comparison` removed from `tukey_hsd()` results (breaking change).
- New column `n` (sample count) added to statistical tests results: `t_test()`, `wilcox_test()`, `sign_test()`, `dunn_test()` and `kruskal_test()` (@ShixiangWang, [#4](https://github.com/kassambara/rstatix/issues/4)).
- `rstatix_test` class added to `anova_test()` results
- the results of `kruskal_test()` is now an object of class `rstatix_test` that has an attribute named **args** for holding the test arguments.
- In `get_y_position()`, y positions and test data are merged now for grouped plots.
- New argument `y.trans` added in `get_y_position()` for y scale transformation.
- significance column added in `tukey_hsd()` results.
- `adjust_pvalue()` now supports grouped data

## Bug fixes
  
- `detailed` arguments correctly propagated when grouped stats are performed

# rstatix 0.1.1
   
   
## New features
  
- New function `get_pvalue_position` added to autocompute p-value positions for plotting significance using ggplot2.
- New function `get_comparisons()` added to create a list of possible pairwise comparisons between groups.
- New function `dunn_test()` added for multiple pairwise comparisons following Kruskal-Wallis test.
- New function `sign_test()` added.

   
## Minor changes
   
- `get_summary_stats()` now supports type = "min", "max", "mean" or "median"
- the results of `t_test()`, `wilcox_test()`, `dunn_test()` and `sign_test()` are now an object of class `rstatix_test` that has an attribute named **args** for holding the test arguments.
- The results of `cohens_d()` is now a data frame containing the Cohen's d and the magnitude.

## Bug fixes
  
- the argument `detatiled` is now passed to `compare_pairs()`.

# rstatix 0.1.0

First release
