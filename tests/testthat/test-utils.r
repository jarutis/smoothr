context("Utilities")

test_that("logspace makes logarithmicaly spaces points", {
  expect_equal(logspace(10, 10000, 4), c(10, 100, 1000, 10000))
  expect_equal(logspace(1, 3, 5), c(1.0, 1.316, 1.732, 2.279, 3.0), tolerance=0.001)
})

test_that("matrix trace is the sum of its diagonal elements", {
  expect_equal(trace(matrix(c(1,2,3,4), 2, 2)), 5)
})
