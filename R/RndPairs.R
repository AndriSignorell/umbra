
#' Generate correlated random pairs
#'
#' Generates pairs of correlated random values and optionally converts them
#' into categorical variables with specified proportions.
#'
#' @param n number of random pairs to generate.
#' @param r desired correlation coefficient between `-1` and `1`.
#' @param rdist1 numeric vector of length `n` containing the values of the
#'   first random variable.
#' @param rdist2 numeric vector of length `n` containing the values of the
#'   second random variable.
#' @param prop `NULL`, a numeric vector of category proportions, or a list
#'   containing separate proportions for the two variables. Proportions must
#'   be positive and sum to one.
#'
#' @return a `data.frame` with the variables `x` and `y`. If `prop` is
#'   specified, both variables are returned as quantile-based factors.
#'
#' @details
#' The second variable is calculated as
#'
#' \deqn{y = r x + \sqrt{1-r^2}z,}
#'
#' where \eqn{x} and \eqn{z} are given by `rdist1` and `rdist2`. If these
#' variables are independent, standardised random variables, the resulting
#' Pearson correlation is approximately `r`.
#'
#' If `prop` is a numeric vector, the same category proportions are applied
#' to both variables. A list of length two can be used to specify different
#' proportions, with the first element referring to `x` and the second to
#' `y`.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' d <- rndPairs(n = 1000, r = 0.7)
#' cor(d$x, d$y)
#'
#' set.seed(42)
#' d <- rndPairs(
#'   n = 1000,
#'   r = 0.5,
#'   prop = c(0.25, 0.50, 0.25)
#' )
#' table(d$x)
#' table(d$y)
rndPairs <- function(n, r,
                     rdist1 = stats::rnorm(n = n),
                     rdist2 = stats::rnorm(n = n),
                     prop = NULL) {
  
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
      n < 1 || n %% 1 != 0) {
    stop("'n' must be a positive integer.", call. = FALSE)
  }
  
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) ||
      r < -1 || r > 1) {
    stop("'r' must be a number between -1 and 1.", call. = FALSE)
  }
  
  if (!is.numeric(rdist1) || length(rdist1) != n ||
      any(!is.finite(rdist1))) {
    stop(
      "'rdist1' must be a finite numeric vector of length 'n'.",
      call. = FALSE
    )
  }
  
  if (!is.numeric(rdist2) || length(rdist2) != n ||
      any(!is.finite(rdist2))) {
    stop(
      "'rdist2' must be a finite numeric vector of length 'n'.",
      call. = FALSE
    )
  }
  
  res <- data.frame(
    x = rdist1,
    y = r * rdist1 + sqrt(1 - r^2) * rdist2
  )
  
  if (is.null(prop))
    return(res)
  
  .checkProp <- function(x, name) {
    
    if (!is.numeric(x) || length(x) == 0L ||
        any(!is.finite(x)) || any(x <= 0)) {
      stop(
        sprintf("'%s' must contain positive finite proportions.", name),
        call. = FALSE
      )
    }
    
    if (abs(sum(x) - 1) > sqrt(.Machine$double.eps)) {
      stop(
        sprintf("The proportions in '%s' must sum to one.", name),
        call. = FALSE
      )
    }
    
    x / sum(x)
  }
  
  if (is.list(prop)) {
    
    if (length(prop) != 2L) {
      stop(
        "'prop' must be a numeric vector or a list of length two.",
        call. = FALSE
      )
    }
    
    propx <- .checkProp(prop[[1L]], "prop[[1]]")
    propy <- .checkProp(prop[[2L]], "prop[[2]]")
    
  } else {
    
    propx <- propy <- .checkProp(prop, "prop")
  }
  
  .categorise <- function(x, proportions, name) {
    
    probs <- c(0, cumsum(proportions))
    probs[length(probs)] <- 1
    
    breaks <- stats::quantile(
      x,
      probs = probs,
      names = FALSE
    )
    
    if (anyDuplicated(breaks)) {
      stop(
        sprintf(
          paste(
            "Quantile breaks for '%s' are not unique.",
            "Use fewer categories or a less discrete input distribution."
          ),
          name
        ),
        call. = FALSE
      )
    }
    
    cutQ(x, breaks = breaks)
  }
  
  res$x <- .categorise(res$x, propx, "x")
  res$y <- .categorise(res$y, propy, "y")
  
  res
}

