context("testing curvature properties of curves")

test_that("total curvature of a unit circle is equal to radian distance", {

  # this is how curvature_poly() works:
  f3 <- function(x) {
    f1 <- Deriv::Deriv(function(x) sqrt(1 - x^2))
    f2 <- Deriv::Deriv(f1)
    ((f2(x)) / ((1 + (f1(x)^2))^1.5)) * # K
      (sqrt(1 + (f1(x))^2)) #ds
  }

  # what is the curvature of a circle between x=1 and x=0 should be pi/2
  int <- abs(integrate(f3, 0, 1)$value)

  expect_equal(int, pi/2)

})


test_that("number of subdivisions doesn't (greatly) affect K", {
  x <- 1:10
  y <- x^2
  mdat <- matrix(c(x, y), nrow = 10, ncol = 2)
  my_poly <- Momocs::npoly(mdat, 2)

  k500 <- curvature_poly(my_poly, c(1, 10), subdiv=500) %>% round(digits=2)
  k1000 <- curvature_poly(my_poly, c(1, 10), subdiv=1000) %>% round(digits=2)

  expect_equal(k500, k1000)
})


test_that("curvature increases for steeper curves", {
  x <- 1:10
  y1 <- x^2
  mdat <- matrix(c(x, y1), nrow = 10, ncol = 2)
  my_poly1 <- Momocs::npoly(mdat, 2)
  k1 <- curvature_poly(my_poly1, c(0, 1))

  y2 <- x^3
  mdat2 <- matrix(c(x, y2), nrow = 10, ncol = 2)
  my_poly2 <- Momocs::npoly(mdat2, 2)
  k2 <- curvature_poly(my_poly2, c(0, 1))

  # x^2 should have greater curvature than x^3 bw [0,1]
  expect_gt(k1, k2)
})


test_that("curvature is not signed", {
  x <- 1:10
  y <- x^2
  mdat <- matrix(c(x, y), nrow = 10, ncol = 2)
  my_poly <- Momocs::npoly(mdat, 2)

  # x^2 from [0,1] and [-1,0] should have equal curvature
  k1 <- curvature_poly(my_poly, c(0, 1)) %>% round(digits = 4)
  k2 <- curvature_poly(my_poly, c(-1, 0)) %>% round(digits = 4)

  expect_equal(k1, k2)
})



