#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr %>% .data
## usethis namespace: end
NULL


# collapsibleTree is imported because we need the htmlwidget defined therein.
# But since we do not use any of its functions, this leads to the following
# warning in R CMD check: All declared Imports should be used.
# The following function makes use of a function from collapsibleTree to
# avoid this warning, but has no other use.
# see: https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports

.ignore_unused_import <- function() {
  collapsibleTree::collapsibleTree
  NULL
}
