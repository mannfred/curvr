context("testing curvature properties of curves")

test_that("total curvature of a unit circle is equal to radian distance", {

  x <- seq(0, 1, by = 0.01)
  y <- sqrt(1-x^2)
  mdat <- matrix(c(x, y), nrow = 101, ncol = 2)

  int1 <-
    curvature_spline(mdat, c(0, sqrt(2)/2)) %>%
    abs() %>%
    round(digits = 3)

  int2 <-
    curvature_spline(mdat, c(0, sqrt(3)/2))  %>%
    abs() %>%
    round(digits = 3)

  expect_equal(int1, round(pi/4, digits=3))
  expect_equal(int2, round(pi/3, digits=3))

})

test_that("total curvature is equal between functions", {

  x <- seq(1, 10, 0.01)
  y <- x^2
  mdat <- matrix(c(x, y), nrow = 901, ncol = 2)
  my_poly <- Momocs::npoly(mdat, 2)

  kpoly <- curvature_poly(my_poly, c(1, 10), subdiv=500) %>% round(digits = 3)
  kspline <- curvature_spline(mdat, c(1, 10)) %>% round(digits = 3)
  ktangle <- curvature_tangle(mdat) %>% round(digits = 3)

  expect_equal(kpoly, kspline)
  expect_equal(kpoly, ktangle)
  expect_equal(kspline, ktangle)
}
