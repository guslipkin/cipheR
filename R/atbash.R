#' Encrypt or decrypt an Atbash Cipher
#'
#' @export
#'
#' @description This can be used to create (encrypt) and solve (decrypt) an
#'   Atbash Cipher. An Atbash Cipher swaps letters' places in the alphabet.
#'   Thus, 'a' becomes 'z', 'b' becomes 'y', and so on. The function does not
#'   differentiate between the two.
#'
#'   The Atbash Cipher Wikipedia entry provides more information on the methods
#'   used: [https://en.wikipedia.org/wiki/Atbash](https://en.wikipedia.org/wiki/Atbash)
#'
#' @param x A vector to be encoded or decoded.
#'
#' @returns A character vector of length one that has been transformed
atbash <- function(x) {

  if (length(x) == 0) {
    stop("Please provide a vector of length greater than zero.")
  } else if (!is.atomic(x) & !is.character(x)) {
    stop("x must be a character vector.")
  }

  x <- strsplit(x, "")

  x <- lapply(x, function(y) {
    y <-
      sapply(y, function(z) {
        if (z %in% c(letters, LETTERS)) {
          if (grepl("[A-Z]", z)) {
            z <- LETTERS[27 - which(LETTERS == z)]
          } else {
            z <- letters[27 - which(letters == z)]
          }
        }
        return(z)
      })
    y <- paste0(y, collapse = "")
    return(y)
  })

  x <- unlist(x, recursive = FALSE)

  return(x)
}
