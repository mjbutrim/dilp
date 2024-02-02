test_that("dilp works", {
  expect_equal(length(dilp(McAbeeExample)), 6)
  expect_equal(as.integer(dilp(McAbeeExample)$results$MAP.MLR[1]), 107)
})
