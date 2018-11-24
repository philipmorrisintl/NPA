#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

context("NPA computation and plots")

test_that("Computed NPA is a valid NPA object", {
  net.apopto <- load_model('Hs', 'CFA', 'Apoptosis')
  npa <- compute_npa(COPD1, net.apopto)
  expect_is( npa, 'NPA' )
  expect_equal( length(comparisons(npa)), 6 )
  
  m <- as.matrix(npa)
  expect_is( m[1, 1], "numeric" )
  
  m <- as.matrix(npa, type="leadingnodes")
  expect_is( m[1, 1], "character" )
  
  d <- npa$get_data()
  for(slot_name in c(
    'coefficients',
    'coefficients.var',
    'ci.up',
    'ci.down')) {
    expect_true( slot_name %in% names(d) )
    expect_is( d[[slot_name]], 'numeric' )
    expect_equal( length(d[[slot_name]]), length(comparisons(npa)) )
  }
  
  for(slot_name in c(
    'nodes.coefficients',
    'nodes.coefficients.ci.up',
    'nodes.coefficients.ci.down',
    'nodes.coefficients.pvalue'
    )) {
    expect_true( slot_name %in% names(d) )
    expect_is( d[[slot_name]], 'matrix' )
    expect_equal( dim(d[[slot_name]]), dim(m) )
  }
})
