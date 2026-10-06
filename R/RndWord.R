
#' Generate random words
#'
#' Generates words of a given length by drawing characters at random, for
#' identifiers, codes and other made-up labels in toy data.
#'
#' @param size number of words to generate.
#' @param length number of characters per word.
#' @param x character vector the characters are drawn from.
#' @param replace logical; may a character occur more than once in a word?
#' @param prob numeric vector of probability weights for the elements of
#'   `x`, or `NULL` for equal weights.
#'
#' @return a character vector of length `size`.
#'
#' @details
#' The successor of `RndWord()` in \pkg{DescTools}. The characters are drawn
#' with [sample()], word by word.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' rndWord(size = 5, length = 4)
#'
#' # codes of two letters and three digits
#' paste0(rndWord(3, 2), rndWord(3, 3, x = 0:9))
rndWord <- function(size, length, x = LETTERS, replace = TRUE, prob = NULL) {
  
  vapply(
    seq_len(size),
    function(i) {
      paste(
        sample(x = x, size = length, replace = replace, prob = prob),
        collapse = ""
      )
    },
    character(1L)
  )
}
