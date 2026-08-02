# Maintainer tasks. In .Rbuildignore, so none of this reaches the CRAN tarball.
#
# Never call pkgdown::build_site() directly -- it renders every root *.md into a
# public page and lists it in sitemap.xml (and bakes its full text into
# search.json where one is built). build-site.sh builds, purges any
# CLAUDE/ISSUE_TEMPLATE artifact from pages, sitemap, search index and llms.txt
# across docs/ and docs/dev/, then FAILS if anything survives.
#
# RSTUDIO_PANDOC points to the DIRECTORY containing the `pandoc` binary
# (the RStudio-bundled pandoc lives at .../MacOS/pandoc/pandoc). Override with
#   make build_site RSTUDIO_PANDOC=/path/to/dir
RSTUDIO_PANDOC ?= /Applications/RStudio.app/Contents/MacOS/pandoc

build_site:
	RSTUDIO_PANDOC="$(RSTUDIO_PANDOC)" .github/scripts/build-site.sh

# Scrub + verify an existing docs/ tree without rebuilding.
scrub_site:
	.github/scripts/build-site.sh --scrub-only

# Kept so `make site` keeps working; it is now the guarded build.
site: build_site

.PHONY: build_site scrub_site site
