#' Plot circle segment that has been fitted a landmark configuration
#'
#'
#' @param landmark_matrix is a \code{matrix} object with \code{[,1]}
#' containing the x landmark coordinates and \code{[,2]} containing
#' the y landmark coordinates.
#'
#' @param x_range the lower and upper x-value bounds to
#' plot the fitted circle. Concatenate the lower and upper bounds using
#' \code{c()}, E.g. for lower = 1 and upper = 10, type \code{c(1,10)}.
#'
#' @param npoints a `numeric` stating how many points to sample from the fitted circle. Default is 1000.
#'
#' @param col a `character` string indicating the colour to plot the
#' points from the fitted circle. Default is 'red'.
#'
#' @param pch a `numeric` indicating the plotting symbol to be used.
#' Default is `1`. See ?graphics::points for details.
#'
#' @param ... further arguments passed to `graphics::points()`.
#'
#' @importFrom pracma circlefit
#'
#' @importFrom graphics points
#'
#' @export

plot_circlefit <- function(landmark_matrix, x_range, npoints=1000, col='red', pch=1, ...) {

  # extract/separate x and y coords
  x_coords <- landmark_matrix[, 1]
  y_coords <- landmark_matrix[, 2]

  # fit a circle using `pracma::circlefit()`
  fitted_circle <- pracma::circlefit(x_coords, y_coords)
  h <- round(fitted_circle[1], digits = 3) # x coordinate of centre
  k <- round(fitted_circle[2], digits = 3) # y coordinate of centre
  r <- round(fitted_circle[3], digits = 3) # radius of fitted circle

  # generate points that lie on the fitted circle
  theta <- seq(0, 2 * pi, length = npoints)
  cx <- r * cos(theta) + h
  cy <- r * sin(theta) + k
  cpoints <- cbind(cx, cy)


  # find two points on circle that minimize distance from f(x) at x_range
  fx_lower <- landmark_matrix[landmark_matrix == x_range[1]]
  fx_upper <- landmark_matrix[landmark_matrix == x_range[2]]

  # compute euclidian distances between curve bounds and fitted circle
  dist_lower <- numeric()
  for (i in 1:nrow(cpoints)) {
    dist_lower[i] <- sqrt((cpoints[i,1] - fx_lower[1])^2 + (cpoints[i,2] - fx_lower[2])^2)
  }

  dist_upper <- numeric()
  for (i in 1:nrow(cpoints)) {
    dist_upper[i] <- sqrt((cpoints[i,1] - fx_upper[1])^2 + (cpoints[i,2] - fx_upper[2])^2)
  }

  # find points on circle that minimize euclidian distances
  clower <- cpoints[which.min(dist_lower),]
  cupper <- cpoints[which.min(dist_upper),]

  # extract circle segment bounded by x_range
  bounded_cpoints <- cpoints[which.min(dist_lower):which.min(dist_upper),]

  plot(landmark_matrix, pch=16)
  points(x=cx,y=cy, col=col, pch=1, ...)

}
