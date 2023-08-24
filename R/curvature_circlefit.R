#' Calculate total curvature from a circle fitted to the landmarks
#'
#'
#' @param landmark_matrix is a \code{matrix} object with \code{[,1]}
#' containing the x landmark coordinates and \code{[,2]} containing
#' the y landmark coordinates.
#'
#' @param x_range the lower and upper x-value bounds to
#' calculate curvature. Concatenate the lower and upper bounds using
#' \code{c()}, E.g. for lower = 1 and upper = 10, type \code{c(1,10)}.
#'
#'
#' @return a `list` with one named element. `$Ktot` is the total curvature in radians.
#'
#' @importFrom pracma circlefit
#'
#' @export

curvature_circlefit <- function(landmark_matrix, x_range) {

  # extract/separate x and y coords
  x_coords <- landmark_matrix[, 1]
  y_coords <- landmark_matrix[, 2]

  # fit a circle using `pracma::circlefit()`
  fitted_circle <- pracma::circlefit(x_coords, y_coords)
  h <- round(fitted_circle[1], digits = 3) # x coordinate of centre
  k <- round(fitted_circle[2], digits = 3) # y coordinate of centre
  r <- round(fitted_circle[3], digits = 3) # radius of fitted circle

  # generate points that lie on the fitted circle
  theta <- seq(0, 2 * pi, length = 1000)
  cx <- r * cos(theta) + h
  cy <- r * sin(theta) + k
  cpoints <- cbind(cx, cy)
  # plot(landmark_matrix)
  # points(x=cx,y=cy, col='blue', pch=16)

  # find two points on circle that minimize distance from f(x) at x_range
  # first, find indicies where `x_range` is in `landmark_matrix`
  fx_lower <- landmark_matrix[landmark_matrix[,1] == x_range[1]]
  fx_upper <- landmark_matrix[landmark_matrix[,1] == x_range[2]]

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

  # compute angle of circle segment
  # from  https://math.stackexchange.com/questions/830413/calculating-the-arc-length-of-a-circle-segment
  d <- sqrt((clower[1] - cupper[1])^2 + (clower[2] - cupper[2])^2)
  Ktot <- acos(1-((d^2)/(2*(r^2))))

  curvature <- list(Ktot = Ktot)
  return(curvature)
}



