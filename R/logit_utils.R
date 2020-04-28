#' @title Logit and inverse logit functions
#'
#' @description Logit and inverse logit functions, with option to perform a
#'   transformation such that the domain in non-logit space is as specified
#'   (standard is (0, 1)).
#'
#' @param x \[`numeric()`\]\cr Value to take the logit or inverse logit of.
#' @param domain_lower \[`numeric()`\]\cr Lower bound of domain in non-logit
#'   space, inclusive. Default 0. Logit at lower bound is -Inf.
#' @param domain_upper \[`numeric()`\]\cr Upper bound of domain in non-logit
#'   space, inclusive. Default 1. Logit at upper bound is Inf.
#'
#' @return \[`numeric()`\]\cr The calculated logit or inverse logit value.
#'
#' @details
#' Standard:
#'
#' * logit (x) = log (x / (1-x))
#' * inverse logit (x) = exp (x) / (1 + exp (x))
#'
#' Optional logit with transformed domain:
#'
#' * logit (x, l, u) = log (x' / (1 - x')) where x' = (x - l) / (u - l)
#' * inverse logit (x, l, u) = ( exp (x) / (1 + exp (x)) ) * (u - l) + l
#'
#' Other notes: Values of x outside of (`domain_lower`, `domain_upper`)
#'   will return NaN and result in a warning from `logit` function.
#'
#' @examples
#' # Standard
#' logit(0.1)
#' invlogit(-2)
#'
#' # Domain shift
#' x <- stats::runif(n = 100, min = 10, max = 20)
#' logit_x <- logit(x, domain_lower = 10, domain_upper = 20)
#' x_again <- invlogit(logit_x, domain_lower = 10, domain_upper = 20)
#'
#' @name logit_utils


# ===================================================================
#' @rdname logit_utils
#' @export
logit <- function(x, domain_lower = 0, domain_upper = 1) {

  check_logit_inputs(x, domain_lower, domain_upper)

  x <- (x - domain_lower) / (domain_upper - domain_lower)
  result <- log(x / (1 - x))

  return(result)
}


# ===================================================================
#' @rdname logit_utils
#' @export
invlogit <- function(x, domain_lower = 0, domain_upper = 1) {

  check_logit_inputs(x, domain_lower, domain_upper)

  result <- exp(x) / (1 + exp(x))
  result <- result * (domain_upper - domain_lower) + domain_lower

  return(result)
}


# ===================================================================
#' @rdname logit_utils
check_logit_inputs <- function(x, domain_lower, domain_upper) {

  # check numeric, input lengths, and lower < upper
  assertive::assert_is_numeric(c(x, domain_lower, domain_upper))
  assertthat::assert_that(length(domain_lower) %in% c(1, length(x)) &
                            length(domain_upper) %in% c(1, length(x)),
                          msg = "`domain_lower` and `domain_upper` must be
                                  length 1 or same length as `x`.")
  assertthat::assert_that(all(domain_lower < domain_upper))
}
