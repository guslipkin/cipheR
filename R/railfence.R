#' Encrypt or decrypt a Railfence Cipher
#'
#' @export
#'
#' @description This can be used to create (encrypt) and solve (decrypt) a
#'   Railfence Cipher. A Railfence Cipher maps each letter to a cosine wave of
#'   the specified height where each letter resides at a specified rail height.
#'   The function does not differentiate between the encryption and decryption.
#'
#' @param x A vector to be encoded or decoded.
#' @param n (Default: `1`) The width of the rail to be used. A width of one will
#'   have no effect.
#' @param decrypt This is not implemented yet
#'
#' @returns A character vector of length one that has been transformed
railfence <- function(x, n = 1, decrypt = FALSE) {

  if(!decrypt) {
    stop("I'm not able to decrypt this yet. Sorry!")
  }

  if (length(x) == 0) {
    stop("Please provide a vector of length greater than zero.")
  } else if (!is.atomic(x) & !is.character(x)) {
    stop("x must be a character vector.")
  }

  if (n < 1) {
    stop("n must be greater than or equal to 1.")
  }

  x <- strsplit(x, "")

  x <- lapply(x, function(y) {
    if (!decrypt) {
      nx <- length(y)
      z <- c(1, rep_len(c(2:n, (n - 1):1), nx - 1))
      y <- data.frame("x" = y, "pos" = z)
      y <- y[order(y$pos),]
      y <- paste0(y$x, collapse = "")
      return(y)
    } else {
      nx <- length(y)
      z <- c(1, rep_len(c(2:n, (n - 1):1), nx - 1))
      y <- data.frame("x" = y, "pos" = z)
      y <- sapply(1:n, function(zz) {

      })
      return(y)
    }
  })

  x <- unlist(x, recursive = FALSE)

  return(x)
}
