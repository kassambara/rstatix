context("test-df-labels")

# Regression tests for #324: df_label_both(), df_label_value() and
# add_panel_label() built their label by pulling the united column out of
# df_unite_factors(), which sorts its input before uniting, then mutated that
# sorted vector back onto the unsorted data. Every row whose sorted position
# differed from its own position got another group's label.
#
# The assertions below are deliberately row-by-row: comparing sorted sets, or
# only the levels, cannot see a permutation of the values.

# Label expected for one row, recomputed from that row's own key values.
expected_both <- function(..., sep = c(", ", ":")) {
  keys <- list(...)
  paste(mapply(function(nm, v) paste(nm, v, sep = sep[2]), names(keys), keys),
        collapse = sep[1])
}

test_that("df_split_by labels each subset with its own group, not a sorted neighbour (#324)", {
  # region is character and not in alphabetical order, so nest order
  # (North, East, South) differs from sorted order (East, North, South)
  df <- data.frame(
    region = rep(c("North", "East", "South"), each = 4),
    value = 1:12,
    stringsAsFactors = FALSE
  )
  res <- df_split_by(df, vars = "region")

  expect_equal(as.character(res$region), c("North", "East", "South"))
  expect_equal(
    as.character(res$label),
    c("region:North", "region:East", "region:South")
  )
  # row-by-row: the label must be built from that row's own key
  for (i in seq_len(nrow(res))) {
    expect_equal(as.character(res$label[i]), expected_both(region = res$region[i]))
    # the subset really does hold that region's rows
    expect_equal(res$data[[i]]$value, which(df$region == res$region[i]))
  }

  # add_panel_name() copies the label into every nested frame, so the same
  # mislabelling reached the subsets themselves
  for (i in seq_len(nrow(res))) {
    expect_equal(
      as.character(res$data[[i]]$label),
      rep(expected_both(region = res$region[i]), nrow(res$data[[i]]))
    )
  }
})

test_that("df_label_both labels each row with its own key (#324)", {
  df <- data.frame(
    region = rep(c("North", "East", "South"), each = 4),
    value = 1:12,
    stringsAsFactors = FALSE
  )

  # on the nested keys
  res <- df_nest_by(df, vars = "region") %>% df_label_both(vars = "region")
  for (i in seq_len(nrow(res))) {
    expect_equal(as.character(res$label[i]), expected_both(region = res$region[i]))
  }

  # and on an ordinary, unsorted data frame
  res2 <- df_label_both(df, vars = "region")
  expect_equal(as.character(res2$region), df$region)
  for (i in seq_len(nrow(res2))) {
    expect_equal(as.character(res2$label[i]), expected_both(region = res2$region[i]))
  }
})

test_that("df_label_both on a factor follows the row, not the alphabet (#324)", {
  # concat_groupname_to_levels() turns the factor into character before the
  # unite, so the sort went alphabetical while nest order followed the levels:
  # this misaligned even when the grouping column was a factor
  df <- data.frame(
    region = factor(rep(c("North", "East", "South"), each = 4),
                    levels = c("North", "East", "South")),
    value = 1:12
  )
  res <- df_nest_by(df, vars = "region") %>% df_label_both(vars = "region")

  expect_equal(as.character(res$region), c("North", "East", "South"))
  for (i in seq_len(nrow(res))) {
    expect_equal(as.character(res$label[i]), expected_both(region = res$region[i]))
  }
})

test_that("df_label_value labels each row with its own key (#324)", {
  df <- data.frame(
    region = rep(c("North", "East", "South"), each = 4),
    value = 1:12,
    stringsAsFactors = FALSE
  )
  res <- df_nest_by(df, vars = "region") %>% df_label_value(vars = "region")
  for (i in seq_len(nrow(res))) {
    expect_equal(as.character(res$label[i]), as.character(res$region[i]))
  }

  res2 <- df_split_by(df, vars = "region", labeller = df_label_value)
  for (i in seq_len(nrow(res2))) {
    expect_equal(as.character(res2$label[i]), as.character(res2$region[i]))
  }
})

test_that("df_label_value on a factor with non-alphabetical levels is unchanged (#324)", {
  # no-regression lock: arrange() on a factor follows its levels, which is
  # already the nest order, so this path was correct before the fix and must
  # stay byte-identical
  df <- data.frame(
    region = factor(rep(c("North", "East", "South"), each = 4),
                    levels = c("North", "East", "South")),
    value = 1:12
  )
  res <- df_nest_by(df, vars = "region") %>% df_label_value(vars = "region")

  expect_equal(as.character(res$label), c("North", "East", "South"))
  expect_equal(levels(res$label), c("North", "East", "South"))
  for (i in seq_len(nrow(res))) {
    expect_equal(as.character(res$label[i]), as.character(res$region[i]))
  }
})

test_that("labelling handles multiple grouping variables (#324)", {
  # ToothGrowth is ordered VC first, so the nest order (0.5/VC, 1/VC, 2/VC,
  # 0.5/OJ, ...) differs from the sorted order (0.5/OJ, 0.5/VC, 1/OJ, ...)
  res <- df_split_by(ToothGrowth, dose, supp)

  expect_equal(nrow(res), 6)
  for (i in seq_len(nrow(res))) {
    expect_equal(
      as.character(res$label[i]),
      expected_both(dose = res$dose[i], supp = res$supp[i])
    )
    expect_equal(
      res$data[[i]]$len,
      ToothGrowth$len[ToothGrowth$dose == res$dose[i] & ToothGrowth$supp == res$supp[i]]
    )
  }

  res2 <- df_label_value(df_nest_by(ToothGrowth, dose, supp), vars = c("dose", "supp"))
  for (i in seq_len(nrow(res2))) {
    expect_equal(
      as.character(res2$label[i]),
      paste(res2$dose[i], res2$supp[i], sep = ", ")
    )
  }
})

test_that("labelling is row-aligned when a value contains the separator (#324)", {
  df <- data.frame(
    g = c("b, x", "b, x", "a, y", "a, y"),
    value = 1:4,
    stringsAsFactors = FALSE
  )
  res <- df_split_by(df, vars = "g")

  expect_equal(as.character(res$g), c("b, x", "a, y"))
  for (i in seq_len(nrow(res))) {
    expect_equal(as.character(res$label[i]), paste0("g:", res$g[i]))
  }
})

test_that("labelling is row-aligned with NA in the grouping column (#324)", {
  df <- data.frame(
    g = c("North", "North", NA, NA, "East", "East"),
    value = 1:6,
    stringsAsFactors = FALSE
  )
  res <- df_split_by(df, vars = "g")

  expect_equal(as.character(res$g), c("North", NA, "East"))
  # unite() pastes a missing value as the string "NA"
  expect_equal(as.character(res$label), c("g:North", "g:NA", "g:East"))
  for (i in seq_len(nrow(res))) {
    expect_equal(
      as.character(res$label[i]),
      paste0("g:", ifelse(is.na(res$g[i]), "NA", res$g[i]))
    )
  }

  resv <- df_split_by(df, vars = "g", labeller = df_label_value)
  expect_equal(as.character(resv$label), c("North", "NA", "East"))
})

test_that("labelling works with a single group (#324)", {
  df <- data.frame(g = rep("only", 4), value = 1:4, stringsAsFactors = FALSE)
  res <- df_split_by(df, vars = "g")

  expect_equal(nrow(res), 1)
  expect_equal(as.character(res$label), "g:only")
  expect_equal(levels(res$label), "g:only")
  expect_equal(as.character(res$data[[1]]$label), rep("g:only", 4))
})

test_that("add_panel_label labels each row with its own key (#324)", {
  # internal helper, currently without a caller inside the package
  df <- data.frame(
    region = rep(c("North", "East", "South"), each = 4),
    value = 1:12,
    stringsAsFactors = FALSE
  )
  res <- add_panel_label(df_nest_by(df, vars = "region"), groups = "region")
  for (i in seq_len(nrow(res))) {
    expect_equal(as.character(res$label[i]), expected_both(region = res$region[i]))
  }
})

test_that("label factor levels keep the order df_unite_factors() produces (#324)", {
  # the fix moves which row gets which label; the level order - and so any
  # downstream panel ordering built on it - must stay the sorted one
  df <- data.frame(
    region = rep(c("North", "East", "South"), each = 4),
    value = 1:12,
    stringsAsFactors = FALSE
  )
  nested <- df_nest_by(df, vars = "region")

  both <- df_label_both(nested, vars = "region")
  expect_s3_class(both$label, "factor")
  expect_equal(levels(both$label), c("region:East", "region:North", "region:South"))

  value <- df_label_value(nested, vars = "region")
  expect_equal(levels(value$label), c("East", "North", "South"))

  # parity with the exported uniting function the labellers used to borrow
  expect_equal(
    levels(value$label),
    levels(df_unite_factors(nested, col = "label", vars = "region")$label)
  )
  expect_equal(
    levels(df_split_by(ToothGrowth, dose, supp)$label),
    levels(df_unite_factors(
      concat_groupname_to_levels(df_select(df_nest_by(ToothGrowth, dose, supp),
                                           vars = c("dose", "supp")),
                                 c("dose", "supp"), sep = ":"),
      col = "label", vars = c("dose", "supp"), sep = ", "
    )$label)
  )
})

test_that("df_unite_factors still sorts before uniting (#324)", {
  # no-regression lock: df_unite_factors() is exported and documented as
  # "First, order factors levels then merge"; the fix must not touch it
  df <- data.frame(
    region = rep(c("North", "East", "South"), each = 4),
    value = 1:12,
    stringsAsFactors = FALSE
  )
  res <- df_unite_factors(df, col = "label", vars = "region")

  expect_equal(
    as.character(res$label),
    rep(c("East", "North", "South"), each = 4)
  )
  expect_equal(levels(res$label), c("East", "North", "South"))
  expect_equal(res$value, c(5:8, 1:4, 9:12))

  # and the untouched df_unite() keeps the input row order
  expect_equal(
    df_unite(df, col = "label", vars = "region")$label,
    rep(c("North", "East", "South"), each = 4)
  )
})
