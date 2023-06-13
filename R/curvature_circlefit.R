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
#' @return a `list` with two named elements. `$Ktot` is the total curvature in radians. `$Ki` is a numeric vector of local curvature values.
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

  # use x-coordinate from `x_range` to determine where on the circle to calculate curvature
  # if (x-h)^2 + (y-k)^2 = r^2 then
  # y^2 -2ky + k^2 - (r^2 - (x-h)^2) = 0, solve for quadratic
  a <- 1
  b <- -2*k
  c1 <- k^2 - r^2 + (x_range[1]-h)^2
  c2 <- k^2 - r^2 + (x_range[2]-h)^2

  # solve quadratic equation function
  # from https://stackoverflow.com/questions/64749481/how-to-solve-a-quadratic-equation-in-r
  quad <- function(a, b, c)
  {
    a <- as.complex(a)
    answer <- c((-b + sqrt(b^2 - 4 * a * c)) / (2 * a),
                (-b - sqrt(b^2 - 4 * a * c)) / (2 * a))
    if(all(Im(answer) == 0)) answer <- Re(answer)
    if(answer[1] == answer[2]) return(answer[1])
    answer
  }

  # lower bound (and find solution that is closest to the graph of f(x))
  y1 <- quad(a, b, c1)
  y1_op <- y1[which.min(y1 - landmark_matrix[1,2])]

  # upper bound
  y2 <- quad(a, b, c2)
  y2_op <- y2[which.min(y2 - landmark_matrix[nrow(landmark_matrix),2])]

  d <- sqrt(sum((c(x_range[1], y1_op) - c(x_range[2], y2_op))^2))
  acos(1-((d^2)/(2*(r^2))))




  curvature <- list(Ktot = theta * (180/pi))
  return(curvature)
}

plot(x_coords, y_coords)
theta <- seq(0, 2 * pi, length = 200)
lines(x = r * cos(theta) + h, y = r * sin(theta) + k, col='red', lwd=3)

