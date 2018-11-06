#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

context("Comparison dataset example")

test_that("Dataset example is in the right format", {
  expect_equal( length(COPD1), 6 )
  expect_is( COPD1, "list" )
  expect_is( COPD1[[1]], "data.frame" )
  expect_true( "nodeLabel" %in% names(COPD1[[1]]))
  expect_true( "foldChange" %in% names(COPD1[[1]]))
  expect_true( "t" %in% names(COPD1[[1]]))
})
