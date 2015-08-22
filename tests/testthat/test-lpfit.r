context("Local Polynomial Kernel Smoothing")

data <- subset(progesterone, group == "nonconceptive" & cycle == 1)
result <- lpfit(progesterone ~ day, data, degree=2)

test_that("lpfit() correctly finds smoothed curved based on LPKS method", {
  fit <-  c(-0.0983, -0.0052, -0.0279, -0.0802, -0.0967, 0.0023, 0.1977, 0.3061,
            0.3350, 0.4741, 0.7535, 1.1119, 1.5447, 2.0393, 2.5198, 2.8786, 3.0165,
            2.9670, 2.8711, 2.8564, 2.9547, 3.0359, 2.7737, 1.8541)
  result_no_bandwidth <- lpfit(progesterone ~ day, data, bandwidth = 1.3, degree=2)
  expect_equal(result_no_bandwidth$estimate, fit, tolerance=0.001)
})

test_that("lpfit() selects good bandwidth", {
  expect_equal(result$bandwidth[1], 1.307, tolerance=0.001)
})

test_that("lpfit() gcv is calculated correctly", {
  gcv <- c(0.0346, 0.0335, 0.0327, 0.0325, 0.0328, 0.0336, 0.0350, 0.0369,
           0.0393, 0.0420, 0.0450, 0.0483, 0.0516, 0.0549, 0.0582)
  expect_equal(attr(result$bandwidth, "candidates")$gcv, gcv, tolerance=0.001)
})

test_that("lpfit() error is calculated correctly", {
  error <- c(0.1230, 0.0801, 0.0802, 0.0778, 0.0766, 0.0763, 0.0763, 0.0763, 
             0.0763, 0.0763, 0.0763, 0.0763, 0.0763, 0.0763, 0.0763, 0.0763, 
             0.0763, 0.0763, 0.0763, 0.0766, 0.0778, 0.0802, 0.0801, 0.1230)
  expect_equal(result$error, error, tolerance=0.001)
})

