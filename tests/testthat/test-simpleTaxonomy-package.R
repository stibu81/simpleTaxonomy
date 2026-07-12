test_that("package startup helpers are side-effect controlled", {
  expect_null(simpleTaxonomy:::.ignore_unused_import())

  old_option <- getOption("simpleTaxonomy_has_igraph_bug")
  on.exit(options(simpleTaxonomy_has_igraph_bug = old_option), add = TRUE)

  options(simpleTaxonomy_has_igraph_bug = NULL)
  simpleTaxonomy:::.onLoad(NULL, NULL)

  expect_identical(
    getOption("simpleTaxonomy_has_igraph_bug"),
    simpleTaxonomy:::has_igraph_bug()
  )
})
