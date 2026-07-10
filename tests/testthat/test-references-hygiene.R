context("test-references-hygiene")

# The packages whose results rstatix compares against while developing. They are
# named in ?rstatix-references and in the relevant function's @details, and they
# are never dependencies. This vector is the single source of truth; the tests
# below keep it, the documentation and the test suite from drifting apart.
cross_check_packages <- c("effectsize", "DescTools", "multcomp",
                          "multcompView", "PMCMRplus")

declared_dependencies <- function() {
  d <- utils::packageDescription("rstatix")
  fields <- paste(c(d$Depends, d$Imports, d$Suggests, d$LinkingTo), collapse = ",")
  out <- trimws(sub("\\(.*", "", unlist(strsplit(fields, ","))))
  out[nzchar(out) & out != "R"]
}

test_files <- function() {
  list.files(".", pattern = "^test.*\\.[Rr]$", full.names = TRUE)
}

# This file names every cross-check package in `cross_check_packages` above, so
# it would vouch for itself if it scanned its own source: a package listed here
# but compared against nowhere would still look "used by a test".
other_test_files <- function() {
  files <- test_files()
  files[basename(files) != "test-references-hygiene.R"]
}

# `multcompView` contains `multcomp` as a substring, so a fixed match on the
# shorter name is satisfied by the longer one. Match on word boundaries instead.
names_package <- function(pkg, txt) {
  any(grepl(paste0("\\b", pkg, "\\b"), txt, perl = TRUE))
}

# The source `man/` directory is preferred, so that a run from a source tree
# checks the Rd files that are actually about to be committed rather than
# whichever version of rstatix happens to be installed. Under `R CMD check` the
# tests run inside `<pkg>.Rcheck/tests`, which has no `man/`, so the database
# falls back to the package being checked -- again the right one.
rd_database <- function() {
  root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
  if (dir.exists(file.path(root, "man"))) {
    db <- tryCatch(tools::Rd_db(dir = root), error = function(e) NULL)
    if (!is.null(db) && length(db)) return(db)
  }
  tryCatch(tools::Rd_db("rstatix"), error = function(e) NULL)
}

test_that("no cross-check package is declared as a dependency", {
  # Adding one to Imports/Suggests would let a test call it, which is a different
  # (and heavier) contract than recording its values.
  expect_equal(intersect(cross_check_packages, declared_dependencies()), character(0))
})

test_that("no Rd file cross-links a package that is not a dependency", {
  # `\link[pkg]{fn}` to a package that is not installed on the checking machine
  # raises an "Unknown package ... in Rd xrefs" NOTE. The cross-check packages ARE
  # installed on a developer's machine, so such a link passes `R CMD check`
  # locally and only fails on CRAN. This test is the only thing that catches it.
  db <- rd_database()
  skip_if(is.null(db) || length(db) == 0, "Rd database not available")

  links <- unlist(lapply(db, function(rd) {
    txt <- paste(utils::capture.output(print(rd)), collapse = "\n")
    regmatches(txt, gregexpr("\\\\link\\[[^]]+\\]", txt))[[1]]
  }))
  targets <- unique(sub("^\\\\link\\[([^]:]+).*$", "\\1", links))
  # `\link[=topic]{text}` links within this package; its bracket holds a topic
  # name, not a package name, and must not be mistaken for an undeclared package.
  targets <- targets[!startsWith(targets, "=")]
  expect_equal(setdiff(targets, c(declared_dependencies(), "rstatix")), character(0))
})

test_that("every cross-check package named in the docs is named by a test", {
  # Guards against listing a package that has left the test suite entirely. This
  # is a text scan: it cannot tell a pinned comparison from a passing mention, so
  # it catches drift, not a fabricated claim. The comparison itself is what the
  # individual test files assert.
  files <- other_test_files()
  skip_if(length(files) == 0, "test sources not available")
  txt <- unlist(lapply(files, readLines, warn = FALSE))
  for (pkg in cross_check_packages) {
    expect_true(names_package(pkg, txt),
                info = paste(pkg, "is named in the documentation but no test names it"))
  }
})

test_that("no undeclared package is referenced by a test without being listed", {
  # Guards the other direction: a new reference implementation added to a test
  # must also be listed in `cross_check_packages` and in ?rstatix-references.
  files <- test_files()
  skip_if(length(files) == 0, "test sources not available")
  txt <- unlist(lapply(files, readLines, warn = FALSE))
  referenced <- unique(unlist(
    regmatches(txt, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=:::?)", txt, perl = TRUE))
  ))
  known <- c(declared_dependencies(), "rstatix",
             rownames(utils::installed.packages(priority = "base")))
  unlisted <- setdiff(referenced, c(known, cross_check_packages))
  expect_equal(unlisted, character(0))
})

test_that("?rstatix-references names every cross-check package", {
  db <- rd_database()
  skip_if(is.null(db) || !("rstatix-references.Rd" %in% names(db)),
          "rstatix-references topic not available")
  txt <- paste(utils::capture.output(print(db[["rstatix-references.Rd"]])), collapse = "\n")
  for (pkg in cross_check_packages) {
    expect_true(names_package(pkg, txt),
                info = paste(pkg, "is used as a cross-check but is absent from ?rstatix-references"))
  }
})
