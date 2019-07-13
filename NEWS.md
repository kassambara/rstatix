# rstatix 0.1.1.999
   
## New features

- New functions added to extract information from statistical tests: `get_anova_label()`
- New function `emmeans_test()` added for pairwise comparisons of estimated marginal means.

## Minor changes
  
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
