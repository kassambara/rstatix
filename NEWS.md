# rstatix 0.1.0.999
   
## To do
   
## New features
  
- New function `dunn_test()` added for multiple pairwise comparisons following Kruskal-Wallis test.
- New function `sign_test()` added.

## Major changes
   
   
## Minor changes
   
- the results of `t_test()`, `wilcox_test()`, `dunn_test()` and `sign_test()` are now an object of class `rstatix_test` that has an attribute named **args** for holding the test arguments.
- The results of `cohens_d()` is now a data frame containing the Cohen's d and the magnitude.

## Bug fixes
  
- the argument `detatiled` is now passed to `compare_pairs()`.

# rstatix 0.1.0

First release
