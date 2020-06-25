context("Calculating curvature correctly")

#  test K for unit circle


test_that("number of subdivisions doesn't (greatly) affect K", {
  x <- 1:10
  y <- x^2
  mdat <- matrix(c(x,y), nrow=10, ncol=2)
  my_poly <- Momocs::npoly(mdat, 2)

  k200   <- total_curvature(my_poly, c(0,1),  200)$total_k %>% round()
  k600   <- total_curvature(my_poly, c(0,1),  600)$total_k %>% round()

expect_equal(k200, k600)
})


test_that("curvature increases for steeper curves", {
  x <- 1:10
  y1 <- x^2
  mdat <- matrix(c(x,y1), nrow=10, ncol=2)
  my_poly1 <- Momocs::npoly(mdat, 2)
  k1 <- total_curvature(my_poly1, c(0,1), 100)$total_k

  y2 <- x^3
  mdat2 <- matrix(c(x,y2), nrow=10, ncol=2)
  my_poly2 <- Momocs::npoly(mdat2, 2)
  k2 <- total_curvature(my_poly2, c(0,1), 100)$total_k

  # x^2 should have greater curvature than x^3 bw [0,1]
  expect_gt(k1, k2)
})


test_that("curvature is not signed", {
  x <- 1:10
  y <- x^2
  mdat <- matrix(c(x,y), nrow=10, ncol=2)
  my_poly <- Momocs::npoly(mdat, 2)

  # x^3 from [0,1] and [-1,0] should have equal curvature
  k1 <- total_curvature(my_poly, c(0, 1), 50)$total_k %>% round(digits = 4)
  k2 <- total_curvature(my_poly, c(-1, 0), 50)$total_k %>% round(digits = 4)

  expect_equal(k1, k2)
})

test_that("curvature of unit circle is 1 at every point", {


# Using the canonical definition of curvature (the inverse radius),
# the curvature of the unit circle is 1 at all points
# (1/r = 1/1 = 1).

# unit circle: 1= x^2 + y^2
# visualize:
# circlefun <- function(x) (1-(x^2))^0.5
# x <- seq(0, 0.9, by=0.01)
# y <- circlefun(x)
# coord <- matrix(c(x, y), nrow=1000, ncol=2)
# plot(coord)

x <- seq(0, 0.9999, by=0.0001)
dfun <- deriv3(expression((1-(x^2))^0.5), "x", func=TRUE)
gr <- attr(dfun(x), "gradient") #computes 1st derivative bw x=0 to x=1
he <- attr(dfun(x), "hessian")[ , , "x"] #computes 2nd derivative bw x=0 to x=1
k <- abs(he)/(1 + gr^2)^(3/2)

expect_equal(k[runif(1, 1, 10000)], 1) #any entry in k should be 1
expect_equal((sum(k) /10000) *(180/pi), 57.29578) #1 radian

})
