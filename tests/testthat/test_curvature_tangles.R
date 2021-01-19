context("testing curvature properties of curves")

test_that("total curvature of a unit circle is equal to radian distance", {

  x <- seq(0, 1, by = 0.1)
  y <- sqrt(1-x^2)
  mdat <- matrix(c(x, y), nrow = length(x), ncol = 2)



  int1 <-
    curvature_tangle(mat, c(0, sqrt(2)/2)) %>%
    abs() %>%
    round(digits = 3)

  int2 <-
    curvature_spline(mdat, c(0, sqrt(3)/2))  %>%
    abs() %>%
    round(digits = 3)

  expect_equal(int1, round(pi/4, digits=3))
  expect_equal(int2, round(pi/3, digits=3))

})
