#' @title Logit and inverse logit functions
#'
#' @param x \[`numeric()`\]\cr Value to take the logit or inverse logit of.
#' @param scale \[`numeric()`\]\cr Value to scale by.
#'
#' @return \[`numeric()`\]\cr The calculated logit or inverse logit
#'  value.
#'
#' @details
#' * logit (x) = log (x / (1-x))
#' * inverse logit (x) = exp (x) / (1 + exp (x))
#'
#' Optional scaled logit with 'scale' argument:
#'
#' * logit (x, s) = s * log (x / (1-x))
#' * inverse logit (x, s) = exp (x/s) / (1 + exp (x/s))
#'
#' @examples
#' logit(0.1)
#' invlogit(-2)
#'
#' @rdname logit_utils

#' @rdname logit_utils
#' @export
logit <- function(x, scale = 1) {
  result <- scale * log(x / (1 - x))
  return(result)
}

#' @rdname logit_utils
#' @export
invlogit <- function(x, scale = 1) {
  x <- x / scale
  result <- exp(x) / (1 + exp(x))
  return(result)
}
