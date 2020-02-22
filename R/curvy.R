#' curvy: Calculate Point-Wise Curvature From Polynomials
#'
#' Calculate curvature from polynomial functions
#' fitted to landmarked specimens. \code{curvy} is designed
#' to pipe from \code{Momocs}, where you can estimate polynomial functions
#' from landmark data.
#'
#' @section curvy functions:
#' The main function of \code{curvy} is \code{totalK()}. This calculates point-wise
#' curvature along bounded polynomials fitted to curved specimens (e.g. cells, bird bills).
#'
#' Calculating curvature, K, involves parameterizing your polynomial function
#' by arclength and computing derivatives. The helper function \code{param()} converts
#' polynomial objects from Momocs into t-parameterized polynomials readable by
#' \code{pracma::arclength()}. \code{func()} allows us to compute the y coordinates
#' of our polynomial once the x-coordinates of the arclength parameterized function
#' are known. \code{express()} converts our Momocs polynomial to an expression
#' so that derivatives can be computed using \code{deriv3()}.
#'
#' @docType package
#' @name curvy
NULL
