context("testing curvature properties of curves")

test_that("total curvature of a unit circle is equal to radian distance", {

  x <- seq(0, 1, by = 0.01)
  y <- sqrt(1-x^2)
  mdat <- matrix(c(x, y), nrow = 101, ncol = 2)

  int1 <-
    curvature_spline(mdat, c(0, sqrt(2)/2), type='smooth')$Ktot %>%
    abs() %>%
    round(digits = 3)

  int2 <-
    curvature_spline(mdat, c(0, sqrt(3)/2), type='smooth')$Ktot  %>%
    abs() %>%
    round(digits = 3)

  int3 <-
    curvature_spline(mdat, c(0, sqrt(2)/2), type='ip')$Ktot %>%
    abs() %>%
    round(digits = 3)

  int4 <-
    curvature_spline(mdat, c(0, sqrt(3)/2), type='ip')$Ktot  %>%
    abs() %>%
    round(digits = 3)

  expect_equal(int1, round(pi/4, digits=3))
  expect_equal(int2, round(pi/3, digits=3))
  expect_equal(int3, round(pi/4, digits=3))
  expect_equal(int4, round(pi/3, digits=3))


})

test_that("total curvature of x^2 between 0 and 1 is arctan of 2", {

  # based on https://math.stackexchange.com/questions/3845048/finding-total-curvature-of-y-x2-x-in0-1
  x <- seq(0, 1, 0.01)
  y <- x^2
  mdat <- matrix(c(x, y), nrow = 101, ncol = 2)

  kspline <- curvature_spline(mdat, c(0, 1), 'ip')$Ktot %>% round(digits = 3)

  expect_equal(kspline, round(atan2(2, 1), digits=3))

})

test_that("total curvature is equal between functions", {

  x <- seq(1, 10, 0.01)
  y <- x^2
  mdat <- matrix(c(x, y), nrow = 901, ncol = 2)

  kspline <- curvature_spline(mdat, c(1, 10), 'ip')$Ktot %>% round(digits = 3)
  ktangle <- curvature_tangle(mdat, iterations=3) %>% round(digits = 3)

  expect_equal(kspline, ktangle)

})
