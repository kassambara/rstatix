context("test-df-labels-collision-order")

# Regression tests for #326, two problems in the df_label_*() labellers:
#
#  1. The final mutate() passed the bare symbol `label`, which dplyr resolves in
#     the data mask first. A grouping column named "label" made the labeller a
#     silent no-op; a data frame merely carrying a "label" column had that
#     column's values written into label_col instead of the computed labels.
#
#  2. df_label_both() took its factor level order from the pasted
#     "<var>:<value>" strings, so a numeric grouping variable ordered
#     lexicographically ("g:10" < "g:2") and a factor's own level order was
#     ignored. df_label_value() already sorted the grouping columns themselves.

# ---------------------------------------------------------------- masking ----

test_that("a grouping column named 'label' is still labelled (#326)", {
  d <- data.frame(label = c("b", "b", "a", "a"), v = 1:4, stringsAsFactors = FALSE)

  both <- df_label_both(d, vars = "label")
  expect_s3_class(both$label, "factor")
  expect_equal(as.character(both$label), c("label:b", "label:b", "label:a", "label:a"))
  expect_equal(levels(both$label), c("label:a", "label:b"))

  value <- df_label_value(d, vars = "label")
  expect_s3_class(value$label, "factor")
  expect_equal(as.character(value$label), c("b", "b", "a", "a"))
  expect_equal(levels(value$label), c("a", "b"))
})

test_that("a pre-existing 'label' column does not leak into label_col (#326)", {
  d <- data.frame(
    g = c("b", "b", "a", "a"),
    label = c("X", "X", "Y", "Y"),
    v = 1:4,
    stringsAsFactors = FALSE
  )

  # label_col is something else: the computed label goes there, and the
  # unrelated "label" column is left alone
  both <- df_label_both(d, vars = "g", label_col = "panel")
  expect_equal(as.character(both$panel), c("g:b", "g:b", "g:a", "g:a"))
  expect_equal(both$label, c("X", "X", "Y", "Y"))

  value <- df_label_value(d, vars = "g", label_col = "panel")
  expect_equal(as.character(value$panel), c("b", "b", "a", "a"))
  expect_equal(value$label, c("X", "X", "Y", "Y"))

  # label_col is the default: the column is overwritten with the label, as
  # documented, rather than being read back as the label
  overwritten <- df_label_both(d, vars = "g")
  expect_equal(as.character(overwritten$label), c("g:b", "g:b", "g:a", "g:a"))
})

test_that("df_split_by and add_panel_label survive a label_col collision (#326)", {
  # grouping BY the column named "label" is the case that reaches the
  # labellers: df_nest_by() keeps it as the key, so the labeller sees a data
  # frame whose only column is called "label" and label_col is "label" too
  d <- data.frame(label = c("b", "b", "a", "a"), v = 1:4, stringsAsFactors = FALSE)

  res <- df_split_by(d, vars = "label")
  expect_equal(as.character(res$label), c("label:b", "label:a"))
  expect_equal(levels(res$label), c("label:a", "label:b"))
  # and it reaches the subsets
  expect_equal(as.character(res$data[[1]]$label), rep("label:b", 2))

  pl <- add_panel_label(df_nest_by(d, vars = "label"), groups = "label")
  expect_equal(as.character(pl$label), c("label:b", "label:a"))

  # a "label" column that is NOT the grouping variable is carried into the
  # subsets by df_nest_by(), so it never reaches the labeller; check it stays
  # intact and label_col is still built from the key
  d2 <- data.frame(
    g = c("b", "b", "a", "a"),
    label = c("X", "X", "Y", "Y"),
    v = 1:4,
    stringsAsFactors = FALSE
  )
  res2 <- df_split_by(d2, vars = "g", label_col = "panel")
  expect_equal(as.character(res2$panel), c("g:b", "g:a"))
  expect_equal(res2$data[[1]]$label, c("X", "X"))
})

test_that("a grouping column named like the internal row index still works (#326)", {
  # unite_factors_in_place() adds a row-index column to take the sort order
  # from; it must not collide with a grouping variable of the same name
  d <- data.frame(
    .rstatix_row = rep(c(10, 2, 1), each = 2),
    v = 1:6,
    check.names = FALSE
  )
  res <- df_label_both(df_nest_by(d, vars = ".rstatix_row"), vars = ".rstatix_row")
  expect_equal(
    as.character(res$label),
    c(".rstatix_row:10", ".rstatix_row:2", ".rstatix_row:1")
  )
  expect_equal(
    levels(res$label),
    c(".rstatix_row:1", ".rstatix_row:2", ".rstatix_row:10")
  )
})

# ------------------------------------------------------------ level order ----

test_that("df_label_both orders a numeric grouping variable numerically (#326)", {
  n <- data.frame(g = rep(c(10, 2, 1), each = 2), v = 1:6)
  lv <- levels(df_label_both(df_nest_by(n, vars = "g"), vars = "g")$label)
  expect_equal(lv, c("g:1", "g:2", "g:10"))   # was g:1, g:10, g:2

  # a realistic dose column
  dose <- data.frame(dose = rep(c(0.5, 1, 2, 10), each = 2), v = 1:8)
  expect_equal(
    levels(df_label_both(df_nest_by(dose, vars = "dose"), vars = "dose")$label),
    c("dose:0.5", "dose:1", "dose:2", "dose:10")
  )
})

test_that("df_label_both follows a factor's own level order (#326)", {
  f <- data.frame(
    g = factor(rep(c("North", "East", "South"), each = 2),
               levels = c("North", "East", "South")),
    v = 1:6
  )
  expect_equal(
    levels(df_label_both(df_nest_by(f, vars = "g"), vars = "g")$label),
    c("g:North", "g:East", "g:South")            # was g:East, g:North, g:South
  )

  # unused levels are dropped, as they always were, but the surviving ones
  # keep the declared order rather than going alphabetical
  u <- data.frame(g = factor(rep(c("b", "a"), each = 2), levels = c("z", "b", "a", "q")), v = 1:4)
  expect_equal(
    levels(df_label_both(df_nest_by(u, vars = "g"), vars = "g")$label),
    c("g:b", "g:a")
  )
})

test_that("df_label_both and df_label_value now order alike (#326)", {
  # the invariant behind the fix: for one grouping variable the two labellers
  # differ only by the "<var>:" prefix, never by the order of the panels
  fixtures <- list(
    numeric = data.frame(g = rep(c(10, 2, 1), each = 2), v = 1:6),
    factor  = data.frame(g = factor(rep(c("North", "East", "South"), each = 2),
                                    levels = c("North", "East", "South")), v = 1:6),
    chr     = data.frame(g = rep(c("North", "East", "South"), each = 2), v = 1:6,
                         stringsAsFactors = FALSE),
    na      = data.frame(g = c("North", "North", NA, NA, "East", "East"), v = 1:6,
                         stringsAsFactors = FALSE)
  )
  for (nm in names(fixtures)) {
    nested <- df_nest_by(fixtures[[nm]], vars = "g")
    expect_equal(
      levels(df_label_both(nested, vars = "g")$label),
      paste0("g:", levels(df_label_value(nested, vars = "g")$label)),
      info = nm
    )
  }
})

test_that("df_label_both sorts NA last, like df_label_value (#326)", {
  # NA used to be pasted to the string "g:NA" before the sort, so the missing
  # group landed alphabetically between g:East and g:North
  d <- data.frame(
    g = c("North", "North", NA, NA, "East", "East"),
    v = 1:6,
    stringsAsFactors = FALSE
  )
  res <- df_label_both(df_nest_by(d, vars = "g"), vars = "g")
  expect_equal(levels(res$label), c("g:East", "g:North", "g:NA"))
  # the values themselves are unchanged and still row-aligned
  expect_equal(as.character(res$label), c("g:North", "g:NA", "g:East"))
})

test_that("a zero-row input gets no phantom label level (#326)", {
  # paste("g", character(0), sep = ":") is "g:", so the old level derivation
  # invented a level for a frame with no rows
  d <- data.frame(g = character(0), v = integer(0), stringsAsFactors = FALSE)
  res <- df_label_both(d, vars = "g")
  expect_equal(nrow(res), 0)
  expect_length(levels(res$label), 0)
})

# ------------------------------------------------------------- grouping ----

test_that("a grouped or rowwise input is labelled like an ungrouped one (#326)", {
  # the label depends only on the row's own grouping values, so a dplyr
  # grouping on some other column must not influence it. The row order used to
  # derive the levels is taken with row_number(), which counts WITHIN groups on
  # a grouped_df; without ungrouping first, the level set is truncated and every
  # row outside it is silently labelled NA.
  d <- data.frame(
    g = c("b", "b", "a", "a"),
    blk = c(1, 2, 1, 2),
    v = 1:4,
    stringsAsFactors = FALSE
  )
  grouped <- dplyr::group_by(d, blk)

  both <- df_label_both(grouped, vars = "g")
  expect_equal(as.character(both$label), c("g:b", "g:b", "g:a", "g:a"))
  expect_equal(levels(both$label), c("g:a", "g:b"))
  expect_false(anyNA(both$label))

  value <- df_label_value(grouped, vars = "g")
  expect_equal(as.character(value$label), c("b", "b", "a", "a"))
  expect_equal(levels(value$label), c("a", "b"))
  expect_false(anyNA(value$label))

  # identical to the ungrouped call, values and levels
  for (f in list(df_label_both, df_label_value)) {
    g <- f(grouped, vars = "g")$label
    u <- f(d, vars = "g")$label
    expect_equal(as.character(g), as.character(u))
    expect_equal(levels(g), levels(u))
  }
})

test_that("grouping does not drop whole groups from the labels (#326)", {
  # a shape where the truncated level set loses an entire group: grouped by
  # supp, the dose == 2 rows fall outside the first group's labels
  tg <- ToothGrowth[!(ToothGrowth$supp == "VC" & ToothGrowth$dose == 2), ]
  res <- df_label_both(dplyr::group_by(tg, supp), vars = "dose")

  expect_false(anyNA(res$label))
  expect_equal(levels(res$label), c("dose:0.5", "dose:1", "dose:2"))
  expect_equal(as.character(res$label), paste0("dose:", tg$dose))
  expect_equal(sum(res$label == "dose:2"), sum(tg$dose == 2))

  # rowwise() indexes one row per group, the most extreme case
  rw <- df_label_both(dplyr::rowwise(ToothGrowth), vars = "dose")
  expect_false(anyNA(rw$label))
  expect_equal(as.character(rw$label), paste0("dose:", ToothGrowth$dose))
  expect_equal(levels(rw$label), c("dose:0.5", "dose:1", "dose:2"))
})

test_that("labelling with no grouping variable errors consistently (#326)", {
  # a label built from no variables has length 0 and cannot be assigned to a
  # 4-row frame. That errored before too - except when the data happened to
  # carry a "label" column, which the old code echoed back and passed off as
  # success. It must not depend on whether such a column is present.
  no_label_col <- data.frame(g = c("b", "b", "a", "a"), v = 1:4, stringsAsFactors = FALSE)
  has_label_col <- data.frame(
    g = c("b", "b", "a", "a"),
    label = c("X", "X", "Y", "Y"),
    v = 1:4,
    stringsAsFactors = FALSE
  )
  expect_error(df_label_both(no_label_col))
  expect_error(df_label_both(has_label_col))
})

# --------------------------------------------------------- no-regression ----

test_that("character grouping and df_unite_factors are unaffected by #326", {
  chr <- data.frame(
    g = rep(c("North", "East", "South"), each = 4),
    v = 1:12,
    stringsAsFactors = FALSE
  )
  # a constant "<var>:" prefix cannot reorder character values, so this case
  # is byte-identical to before the change
  expect_equal(
    levels(df_label_both(df_nest_by(chr, vars = "g"), vars = "g")$label),
    c("g:East", "g:North", "g:South")
  )
  expect_equal(
    levels(df_split_by(ToothGrowth, dose, supp)$label),
    c("dose:0.5, supp:OJ", "dose:0.5, supp:VC", "dose:1, supp:OJ",
      "dose:1, supp:VC", "dose:2, supp:OJ", "dose:2, supp:VC")
  )

  # the exported uniting function keeps sorting on the columns it is given,
  # including the lexicographic order for a numeric column
  n <- data.frame(g = rep(c(10, 2, 1), each = 2), v = 1:6)
  uf <- df_unite_factors(n, col = "lab", vars = "g")
  expect_equal(as.character(uf$lab), rep(c("1", "2", "10"), each = 2))
  expect_equal(levels(uf$lab), c("1", "2", "10"))
})
