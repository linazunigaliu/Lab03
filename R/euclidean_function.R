#' Euclidean Algorithm
#'
#' This function implements the **Euclidean algorithm** to compute the
#' greatest common divisor of two integers. The Euclidean algorithm
#' is based on the principle that the greatest common divisor of two numbers
#' also divides their remainder when one is divided by the other.
#'
#' @param a A numeric integer scalar.
#' @param b A numeric integer scalar.
#'
#' @return An integer value representing the greatest common divisor
#' of \code{a} and \code{b}.
#'
#' @details
#' The function first checks that both inputs are integers.
#' Negative numbers are converted to their absolute values.
#' The algorithm repeatedly replaces the larger number with the remainder
#' of dividing the larger by the smaller until the remainder is zero.
#' At that point, the smaller number is the greatest common divisor.
#'
#' @references
#' Euclidean Algorithm explanation: \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @examples
#' euclidean(123612, 13892347912)  # returns 4
#' euclidean(100, 1000) # returns 100
#'
#' @export
euclidean <- function(a,b){

  if (!is.numeric(a) || a %% 1 != 0) {
    stop("a must be a numeric integer scalar")
  }

  if (!is.numeric(b) || b %% 1 != 0) {
    stop("b must be a numeric integer scalar")
  }

  if (a < 0) {
    a <- a *-1
  }

  if (b < 0) {
    b <- b *-1
  }

  a_0 <- max(a,b)
  b_0 <- min(a,b)
  r <- a_0 %% b_0
  q <- (a_0 - r) / b_0

  if (r == 0) {

    return(b_0)

  } else {

    repeat{

      a_0 <- b_0
      b_0 <- r
      r <- a_0 %% b_0
      q <- (a_0 - r) / b_0

      if (r == 0) {
        break()
      }

    }

    return(b_0)
  }

}
