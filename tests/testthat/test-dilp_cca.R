test_that("plot works", {
  expect_equal(length(dilp_cca(dilp(McAbeeExample))$layers), 3)
})
