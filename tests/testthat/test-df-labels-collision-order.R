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

test_that("grouping works with more than one labelling variable (#326)", {
  # select() on a grouped_df prepends the grouping columns, so with two
  # labelling variables concat_groupname_to_levels() was handed three columns
  # against two names. This is the repeated-measures idiom: group by subject,
  # label by two factors.
  dd <- data.frame(
    id = rep(1:4, each = 3),
    treat = rep(c("B", "A"), 6),
    time = rep(c(3, 1, 10), 4),
    stringsAsFactors = FALSE
  )
  grouped <- df_label_both(dplyr::group_by(dd, id), treat, time)
  plain <- df_label_both(dd, treat, time)

  expect_false(anyNA(grouped$label))
  expect_equal(as.character(grouped$label), as.character(plain$label))
  expect_equal(levels(grouped$label), levels(plain$label))
  expect_equal(
    as.character(plain$label),
    paste0("treat:", dd$treat, ", time:", dd$time)
  )
  # the composite key still orders time numerically, not as text
  expect_equal(
    levels(plain$label),
    c("treat:A, time:1", "treat:A, time:3", "treat:A, time:10",
      "treat:B, time:1", "treat:B, time:3", "treat:B, time:10")
  )
  # and grouping adds no message the ungrouped call does not raise. Asserted
  # for both labellers: only df_label_both() breaks outright without the
  # ungroup(), so df_label_value()'s would otherwise be unlocked
  expect_no_message(df_label_both(dplyr::group_by(dd, id), treat, time))
  expect_no_message(df_label_value(dplyr::group_by(dd, id), treat, time))

  expect_equal(
    as.character(df_label_value(dplyr::group_by(dd, id), treat, time)$label),
    as.character(df_label_value(dd, treat, time)$label)
  )
})

test_that("df_label_both with no grouping variable behaves as in 1.1.0 (#326)", {
  # df_label_both() cannot build a "<variable>:<value>" label without a variable
  # name, so a zero-length label falls out and dplyr rejects it on a frame with
  # rows. The message is dplyr's and is deliberately not pinned. What matters is
  # the outcome, and that it no longer depends on whether the data happens to
  # carry a "label" column for the old code to echo back.
  no_label_col <- data.frame(g = c("b", "b", "a", "a"), v = 1:4, stringsAsFactors = FALSE)
  has_label_col <- data.frame(
    g = c("b", "b", "a", "a"),
    label = c("X", "X", "Y", "Y"),
    v = 1:4,
    stringsAsFactors = FALSE
  )
  for (d in list(no_label_col, has_label_col)) {
    expect_error(df_label_both(d))
    expect_error(df_split_by(d))
  }

  # a zero-row frame takes a zero-length label without complaint, as it did in
  # 1.1.0 - an assertion here would turn a working call into an error (#328)
  empty <- data.frame(g = character(0), v = integer(0), stringsAsFactors = FALSE)
  expect_equal(as.character(df_label_both(empty, vars = character(0))$label), character(0))
  expect_equal(nrow(df_split_by(empty, vars = character(0))), 0)
})

test_that("df_label_value accepts a zero-length grouping vector (#328)", {
  # labelling by nothing yields an empty label from this labeller, as it has
  # since 1.1.0. ggpubr's free-panel path depends on it: a facet.by that
  # resolves to no column at all must still draw one panel, not error.
  # Only the character(0) case regressed - vars = "" has length 1 and never
  # reached the assertion - but both are locked, the "" one as a neighbour.
  d <- data.frame(g = rep(c("a", "b"), each = 4), v = 1:8, stringsAsFactors = FALSE)

  for (vs in list(character(0), "")) {
    res <- df_label_value(d, vars = vs)
    expect_equal(nrow(res), 8)
    expect_s3_class(res$label, "factor")
    expect_equal(as.character(res$label), rep("", 8))
    expect_equal(levels(res$label), "")
  }

  # and through df_split_by(), which is how ggpubr reaches it
  for (vs in list(character(0), "")) {
    res <- df_split_by(d, vars = vs, label_col = "panel", labeller = df_label_value)
    expect_equal(nrow(res), 1)
    expect_equal(as.character(res$panel), "")
    expect_equal(levels(res$panel), "")
  }
})

# --------------------------------------------------------- no-regression ----

test_that("a missing value sorts last rather than as the text 'NA' (#326)", {
  # the old sort compared each pasted label against the literal string "g:NA",
  # so the missing group only moves when a present value sorts after it in byte
  # order; where every value already sorted before "NA" it was last anyway
  lv <- function(g) levels(df_label_both(data.frame(g = g, v = seq_along(g)),
                                         vars = "g")$label)
  # moves: a present value sorts after "NA"
  expect_equal(lv(c("north", NA, "east")), c("g:east", "g:north", "g:NA"))
  expect_equal(lv(c("OJ", NA, "VC")), c("g:OJ", "g:VC", "g:NA"))
  expect_equal(lv(factor(c("a", NA, "b"), levels = c("a", "b"))),
               c("g:a", "g:b", "g:NA"))       # was g:NA, g:a, g:b
  # does not move: every present value already sorted before "NA"
  expect_equal(lv(c("Control", NA, "Drug")), c("g:Control", "g:Drug", "g:NA"))
  expect_equal(lv(c("A", NA, "B")), c("g:A", "g:B", "g:NA"))
  expect_equal(lv(c(TRUE, NA, FALSE)), c("g:FALSE", "g:TRUE", "g:NA"))
})

test_that("a factor whose levels defy byte order keeps its declared order (#326)", {
  # the old order came from a byte-order sort of the pasted strings, which puts
  # every uppercase initial before every lowercase one. The levels are declared
  # explicitly here on purpose: factor() would otherwise build them with a
  # locale-aware sort, and under the C collation R CMD check runs in, the
  # declared order would already BE the byte order and nothing would move.
  lv <- function(g) levels(df_label_both(data.frame(g = g, v = seq_along(g)),
                                         vars = "g")$label)
  expect_equal(
    lv(factor(c("control", "Treatment"), levels = c("control", "Treatment"))),
    c("g:control", "g:Treatment")             # was g:Treatment, g:control
  )
  expect_equal(
    lv(factor(c("a", "Z"), levels = c("a", "Z"))),
    c("g:a", "g:Z")                           # was g:Z, g:a
  )
  expect_equal(
    lv(factor(c("banana", "Apple"), levels = c("banana", "Apple"))),
    c("g:banana", "g:Apple")                  # was g:Apple, g:banana
  )
  # where the declared order already matches byte order, nothing moves - mixed
  # case on its own does not decide it, the order does
  expect_equal(
    lv(factor(c("Control", "Drug"), levels = c("Control", "Drug"))),
    c("g:Control", "g:Drug")
  )
  expect_equal(
    lv(factor(c("Apple", "banana"), levels = c("Apple", "banana"))),
    c("g:Apple", "g:banana")
  )
})

test_that("character grouping without NA, and df_unite_factors, are unaffected by #326", {
  chr <- data.frame(
    g = rep(c("North", "East", "South"), each = 4),
    v = 1:12,
    stringsAsFactors = FALSE
  )
  # a constant "<var>:" prefix cannot reorder character values, so a character
  # column with no missing value is byte-identical to before the change
  expect_equal(
    levels(df_label_both(df_nest_by(chr, vars = "g"), vars = "g")$label),
    c("g:East", "g:North", "g:South")
  )
  expect_equal(
    levels(df_split_by(ToothGrowth, dose, supp)$label),
    c("dose:0.5, supp:OJ", "dose:0.5, supp:VC", "dose:1, supp:OJ",
      "dose:1, supp:VC", "dose:2, supp:OJ", "dose:2, supp:VC")
  )

  # the exported uniting function keeps sorting on the columns it is given.
  # It always ordered a numeric column numerically, which is what df_label_both()
  # now agrees with: the divergence was on df_label_both()'s side, because it
  # sorted the pasted strings instead of the values
  n <- data.frame(g = rep(c(10, 2, 1), each = 2), v = 1:6)
  uf <- df_unite_factors(n, col = "lab", vars = "g")
  expect_equal(as.character(uf$lab), rep(c("1", "2", "10"), each = 2))
  expect_equal(levels(uf$lab), c("1", "2", "10"))
})
