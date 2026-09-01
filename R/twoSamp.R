
#' Generate two random samples
#'
#' Generates two random samples and returns them in long or wide format.
#'
#' @param n numeric vector containing one common or two separate sample sizes.
#' @param rdist random-generating function or list of two random-generating
#'   functions. Each function must accept the sample size as its first
#'   argument and return a numeric vector of that length.
#' @param groups vector of length two containing the group values.
#' @param valueName name of the value variable in long format.
#' @param groupName name of the group variable in long format.
#' @param output output format, either `"long"` or `"wide"`.
#'
#' @return a `data.frame` containing the two generated samples. In wide
#'   format, the shorter sample is padded with missing values.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#'
#' d <- twoSamp(
#'   n = c(100, 120),
#'   rdist = list(
#'     function(n) rnorm(n, mean = 70, sd = 8),
#'     function(n) rnorm(n, mean = 73, sd = 9)
#'   ),
#'   groups = c("2023", "2024"),
#'   valueName = "gewicht",
#'   groupName = "jahr"
#' )
twoSamp <- function(n, rdist, groups = 1:2,
                    valueName = "value", groupName = "group",
                    output = c("long", "wide")) {
  
  output <- match.arg(output)
  
  if (!is.numeric(n) || length(n) < 1L || length(n) > 2L ||
      any(!is.finite(n)) || any(n < 1) || any(n %% 1 != 0)) {
    stop(
      "'n' must contain one or two positive integers.",
      call. = FALSE
    )
  }
  
  if (length(n) == 1L)
    n <- rep(n, 2L)
  
  if (is.function(rdist))
    rdist <- rep(list(rdist), 2L)
  
  if (!is.list(rdist) || length(rdist) < 1L || length(rdist) > 2L ||
      !all(vapply(rdist, is.function, logical(1L)))) {
    stop(
      "'rdist' must be a function or a list of one or two functions.",
      call. = FALSE
    )
  }
  
  if (length(rdist) == 1L)
    rdist <- rep(rdist, 2L)
  
  if (length(groups) != 2L || anyNA(groups) || anyDuplicated(groups)) {
    stop(
      "'groups' must contain two distinct, non-missing values.",
      call. = FALSE
    )
  }
  
  if (!is.character(valueName) || length(valueName) != 1L ||
      is.na(valueName) || !nzchar(valueName)) {
    stop(
      "'valueName' must be a non-empty character string.",
      call. = FALSE
    )
  }
  
  if (!is.character(groupName) || length(groupName) != 1L ||
      is.na(groupName) || !nzchar(groupName)) {
    stop(
      "'groupName' must be a non-empty character string.",
      call. = FALSE
    )
  }
  
  if (identical(valueName, groupName)) {
    stop(
      "'valueName' and 'groupName' must be different.",
      call. = FALSE
    )
  }
  
  values <- Map(
    function(fun, size) unname(fun(size)),
    rdist,
    n
  )
  
  valid <- vapply(
    seq_along(values),
    function(i) {
      is.numeric(values[[i]]) &&
        length(values[[i]]) == n[i] &&
        all(is.finite(values[[i]]))
    },
    logical(1L)
  )
  
  if (!all(valid)) {
    stop(
      paste(
        "Each function in 'rdist' must return a finite numeric",
        "vector of the requested length."
      ),
      call. = FALSE
    )
  }
  
  if (output == "long") {
    
    res <- data.frame(
      group = rep(groups, times = n),
      value = unlist(values, use.names = FALSE)
    )
    
    names(res) <- c(groupName, valueName)
    
  } else {
    
    maxN <- max(n)
    
    values <- lapply(
      values,
      function(x) {
        length(x) <- maxN
        x
      }
    )
    
    res <- as.data.frame(
      values,
      optional = TRUE,
      check.names = FALSE
    )
    
    names(res) <- as.character(groups)
  }
  
  res
}