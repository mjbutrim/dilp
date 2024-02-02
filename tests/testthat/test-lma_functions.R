test_that("lma works", {
  expect_equal(length(lma(McAbeeExample)), 4)
  expect_equal(nrow(lma(McAbeeExample)$species_mean_lma), 27)
})
