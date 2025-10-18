# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**rstatix** is a pipe-friendly R package providing a tidy framework for basic statistical tests. It wraps base R statistical functions (t-test, ANOVA, Wilcoxon, correlation, etc.) to:
- Return tidy data frames compatible with tidyverse workflows
- Support the `%>%` pipe operator
- Work seamlessly with grouped data (`dplyr::group_by`)
- Facilitate visualization with ggpubr

The package is designed for researchers and data scientists who want to perform statistical tests in a tidy, reproducible manner.

## Development Commands

### Building and Documentation
```r
# Generate documentation from roxygen2 comments
devtools::document()

# Build package tarball
devtools::build()
# Or: R CMD build .

# Install package locally
devtools::install()
```

### Testing
```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-t_test.R")

# Run tests with coverage
covr::package_coverage()
```

### Quality Checks
```r
# Full R CMD check (same as CRAN submission)
devtools::check()

# Or via command line
R CMD build .
R CMD check rstatix_*.tar.gz

# Check for common issues
goodpractice::gp()
```

### Documentation Website
```r
# Build pkgdown site (output in docs/)
pkgdown::build_site()

# Preview locally
pkgdown::preview_site()

# Build single reference page
pkgdown::build_reference()
```

### Releasing
```r
# Check if ready for CRAN
devtools::check_win_devel()    # Windows check
devtools::check_rhub()          # Multiple platform check

# Update NEWS.md and cran-comments.md before submission
devtools::submit_cran()
```

## Architecture

### Core Design Pattern

All statistical test functions follow this pattern:

1. **Main Function** (e.g., `t_test()`, `wilcox_test()`)
   - Validates inputs and extracts formula components
   - Determines if one-sample, two-sample, or pairwise test
   - Delegates to helper functions in `utilities_two_sample_test.R`
   - Returns tidy data frame with class attributes

2. **Helper Functions** (`two_sample_test()`, `pairwise_two_sample_test()`)
   - Handle grouped data via `doo()` function
   - Call base R statistical functions
   - Transform results into tidy format using `broom::tidy()`
   - Add columns: `.y.`, `group1`, `group2`, `n`, `statistic`, `df`, `p`

3. **Attribute System**
   - Test arguments stored via `set_attrs(args = args)`
   - Enables reproducibility and extraction via `get_test_arguments()`
   - Class hierarchy: `c("rstatix_test", "t_test", "tbl_df", ...)`

### File Organization

**R/utilities.R** (748 lines) - Central utility functions:
- NSE helpers: `get_quo_vars()`, `get_quo_vars_list()`
- Formula parsing: `get_formula_left_hand_side()`, `get_formula_right_hand_side()`
- Data manipulation: `as_factor()`, `add_columns()`, `round_value()`
- Grouping: `guess_number_of_groups()`, `get_levels()`
- Correlation matrix utilities
- Class management: `add_class()`, `remove_class()`, `keep_only_tbl_df_classes()`

**R/utilities_two_sample_test.R** - Common logic for two-sample tests:
- `two_sample_test()`: Handles one-sample and two-sample comparisons
- `pairwise_two_sample_test()`: Handles >2 groups with pairwise comparisons
- Supports `ref.group` (compare to reference) and `comparisons` (specific pairs)

**Statistical Test Functions** (R/t_test.R, R/wilcox_test.R, etc.):
- Use `@include utilities.R utilities_two_sample_test.R` to ensure load order
- Follow consistent API: `formula`, `ref.group`, `p.adjust.method`, `comparisons`
- Return objects with class `c("rstatix_test", "{test_name}")`

**Collate Order** (DESCRIPTION lines 47-113):
- Controls file loading sequence
- `utilities.R` loaded first, then test-specific files
- Order matters for `@include` directives

### Key Patterns

**Formula Interface**:
```r
# outcome ~ group
df %>% t_test(len ~ supp)

# One-sample test (compare to mu)
df %>% t_test(len ~ 1, mu = 0)

# Grouped analysis
df %>%
  group_by(dose) %>%
  t_test(len ~ supp)
```

**Pairwise Comparisons**:
- Automatically triggered when `group` has >2 levels
- `ref.group = "0.5"` → compare each level to "0.5"
- `ref.group = "all"` → compare each level to grand mean
- `comparisons = list(c("A", "B"), c("B", "C"))` → specific pairs only

**P-value Adjustment**:
- All pairwise tests support `p.adjust.method` (default: "holm")
- Methods: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
- For grouped data, adjustment done independently per group

**Grouped Data Handling**:
- Uses `doo()` function (alternative to `dplyr::do()`)
- Pattern: `nest() + mutate() + map()` to apply function to each group
- Results automatically unnested with group columns preserved

**Non-Standard Evaluation (NSE)**:
- Uses rlang for modern tidy evaluation
- `get_quo_vars()` extracts variable names from quosures
- `tidyselect::vars_select()` for selecting columns
- Supports both standard eval (character strings) and NSE (bare names)

### Integration with ggpubr

rstatix is designed to work with ggpubr for visualization:
```r
# Workflow: test → visualize
stat.test <- df %>% t_test(len ~ supp)

ggboxplot(df, x = "supp", y = "len") +
  stat_pvalue_manual(stat.test, label = "p")
```

Data manipulation helpers (`df_select()`, `df_group_by()`, etc.) are used internally by both packages.

## Common Development Tasks

### Adding a New Statistical Test

1. Create `R/new_test.R`:
```r
#' @include utilities.R utilities_two_sample_test.R
NULL

#'@export
new_test <- function(data, formula, ...) {
  args <- as.list(environment())
  params <- args %>%
    remove_null_items() %>%
    add_item(method = "base.r.function")

  # Determine test type
  number.of.groups <- guess_number_of_groups(data, group)
  test.func <- if(number.of.groups > 2) pairwise_two_sample_test else two_sample_test

  do.call(test.func, params) %>%
    set_attrs(args = args) %>%
    add_class(c("rstatix_test", "new_test"))
}
```

2. Add to DESCRIPTION Collate field (maintain order)

3. Write tests in `tests/testthat/test-new_test.R`:
```r
test_that("new_test works with two groups", {
  result <- mtcars %>% new_test(mpg ~ am)
  expect_s3_class(result, "rstatix_test")
  expect_true("p" %in% colnames(result))
})
```

4. Update `_pkgdown.yml` to include in documentation

### Modifying Existing Tests

1. Test functions are in `R/{test_name}.R`
2. Run specific test: `testthat::test_file("tests/testthat/test-{name}.R")`
3. If changing return structure, update `get_test_label()` in `R/get_test_label.R`
4. If adding parameters, update roxygen documentation

### Debugging Grouped Operations

```r
# Enable verbose output in doo()
df %>%
  group_by(dose) %>%
  t_test(len ~ supp) %>%
  print()

# Extract test arguments
result <- df %>% t_test(len ~ supp)
attr(result, "args")

# Check grouping variables
get_test_grouping_vars(result)
```

### Working with Correlation Functions

Correlation functions (`cor_test()`, `cor_mat()`, etc.) use a different pattern:
- Return objects with class `cor_mat` or `cor_test`
- Store p-values as attributes: `attr(cor_mat, "pvalue")`
- Special utilities in utilities.R: `subset_matrix()`, `as_matrix()`, `matrix_to_tibble()`

## Testing Strategy

### Test Structure
- Tests in `tests/testthat/test-*.R`
- Use built-in datasets: `ToothGrowth`, `mtcars`, `iris`
- Test both ungrouped and grouped data
- Verify column names, classes, and statistical correctness

### Key Assertions
```r
# Check class hierarchy
expect_s3_class(result, c("rstatix_test", "t_test"))

# Verify columns
expect_true(all(c(".y.", "group1", "group2", "p") %in% names(result)))

# Check attributes
expect_true(!is.null(attr(result, "args")))

# Grouped results should have group columns
expect_true("dose" %in% names(grouped_result))
```

## Documentation Standards

### Roxygen2 Structure
```r
#' @include utilities.R      # Load dependencies first
#'
#' Brief Title
#'
#' @description Detailed description with links
#' @inheritParams stats::base_function
#' @param data a data.frame containing variables
#' @param formula outcome ~ group
#' @return data frame with columns: .y., group1, group2, p, ...
#' @examples
#' # One-sample test
#' df %>% t_test(len ~ 1, mu = 0)
#' @export
```

### pkgdown Configuration
- `_pkgdown.yml` organizes reference documentation
- Sections: "Descriptive Statistics", "Comparing Means", "Post-Hoc", etc.
- Automatically generates website at https://rpkgs.datanovia.com/rstatix/

## Package Dependencies

### Imports (Required)
- **tidyverse ecosystem**: dplyr, tidyr, purrr, tibble, tidyselect, rlang
- **Statistical**: stats, broom, car, generics
- **Other**: magrittr (%>%), corrplot, utils

### Suggests (Optional)
- **Documentation**: knitr, rmarkdown
- **Testing**: testthat, spelling
- **Integration**: ggpubr, emmeans, coin, boot

Minimize adding new dependencies - prefer base R solutions when possible.

## CI/CD

GitHub Actions workflow (`.github/workflows/R-CMD-check.yaml`):
- Runs on: macOS, Windows, Ubuntu (R-devel, R-release, R-oldrel)
- Triggered by: push to main/master, pull requests
- Checks: R CMD check with full test suite

## CRAN Submission Checklist

Before submitting to CRAN:
1. Update `DESCRIPTION` version (x.y.z → x.y.(z+1) for patches)
2. Update `NEWS.md` with user-facing changes
3. Fill `cran-comments.md` with submission notes
4. Run `devtools::check()` with 0 errors, warnings, notes
5. Test on multiple platforms via `rhub::check_for_cran()`
6. Verify all URLs in documentation work
7. Check backwards compatibility
8. Submit via `devtools::submit_cran()`

## Resources

- Package website: https://rpkgs.datanovia.com/rstatix/
- GitHub: https://github.com/kassambara/rstatix
- Tutorials: https://www.datanovia.com/en/courses/comparing-means-in-r/
- Companion package: ggpubr (visualization)
