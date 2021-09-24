context("testing curvature properties of curves")

test_that("total curvature of a unit circle is equal to radian distance", {

  x1 <- seq(0, sqrt(2)/2, length.out=9)
  y1 <- sqrt(1-x1^2)
  mdat1 <- matrix(c(x1, y1), nrow = length(x1), ncol = 2)

  # need to improve precision at least to ~ digits = 3
  int1 <-
    curvature_tangle(mdat1, 10) %>%
    abs() %>%
    round(digits = 3)

  x2 <- seq(0, sqrt(3)/2, length.out=9)
  y2 <- sqrt(1-x2^2)
  mdat2 <- matrix(c(x2, y2), nrow = length(x2), ncol = 2)

  int2 <-
    curvature_tangle(mdat2, 10)  %>%
    abs() %>%
    round(digits = 3)

  # to do: improve precision
  expect_equal(int1, round(pi/4, digits=3), tolerance=0.02)
  expect_equal(int2, round(pi/3, digits=3), tolerance=0.02)

})
