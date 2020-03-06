context("Converting and formatting functions")


test_that("as_expression() creates expressions", {
  x <- 1:10
  y <- x^2
  mdat <- matrix(c(x,y), nrow=10, ncol=2)
  my_poly <- Momocs::npoly(mdat, 2)
  expect_equal(eval(as_expression(my_poly)), eval(expression(x^2)))
})


test_that("as_param() parameterizes a Momocs polynomial", {
  x <- 1:10
  y <- x^2
  mdat <- matrix(c(x,y), nrow=10, ncol=2)
  my_poly1 <- Momocs::npoly(mdat, 2)

  my_poly2 <- list(c(0,0,1)) #numeric vector of coeffs for y= 0 + 0x^1 +1x^2
  expect_equal(pracma::arclength(as_param(my_poly1), 0, 1),
               pracma::arclength(as_param(my_poly2), 0, 1))
})


test_that("as_func() creates functions", {
  x <- 1:10
  y <- x^2
  mdat <- matrix(c(x,y), nrow=10, ncol=2)
  my_poly <- Momocs::npoly(mdat, 2)
  my_func1 <- as_func(my_poly)

  my_func2 <- function(x) x^2
  expect_equal(my_func1(1.5), my_func2(1.5))
  expect_equal(my_func1(-3.5), my_func2(-3.5))
})
