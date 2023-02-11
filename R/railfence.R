#' Encrypt or decrypt a Railfence Cipher
#'
#' @export
#'
#' @description This can be used to create (encrypt) and solve (decrypt) a
#'   Railfence Cipher. A Railfence Cipher maps each letter to a cosine wave of
#'   the specified height where each letter resides at an integer rail height.
#'
#'   The Railfence Cipher Wikipedia entry provides more information on the methods
#'   used: [https://en.wikipedia.org/wiki/Rail_fence_cipher](https://en.wikipedia.org/wiki/Rail_fence_cipher)
#'
#' @param x A vector to be encoded or decoded.
#' @param n (Default: `1`) The width of the rail to be used. A width of one will
#'   have no effect.
#' @param decrypt (Default: `FALSE`) The default `FALSE` will encrypt while
#'   using `TRUE` will decrypt a given value of `x`.
#'
#' @returns A character vector of length one that has been transformed
#' @examples
#' (e1 <- railfence("abcde", 2))
#' railfence(e1, 2, decrypt = TRUE)
#'
#' (e2 <- railfence("cipheR is a great R package!", 4))
#' railfence(e2, 4, decrypt = TRUE)
#'
#' (e3 <- railfence("Isn't this fun?", 3))
#' railfence(e3, 3, decrypt = TRUE)
railfence <- function(x, n = 1, decrypt = FALSE) {

  if (length(x) == 0) {
    stop("Please provide a vector of length greater than zero")
  } else if (!is.atomic(x) & !is.character(x)) {
    stop("x must be a character vector.")
  }

  if (length(n) != 1 || n < 1 || n %% 1 != 0) {
    stop("n must be a single integer greater than or equal to 1")
  }

  if (!is.logical(decrypt)) {
    stop("decrypt must be logical")
  }

  # dispatch the encryption or decryption method
  if (!decrypt) {
    x <- .railfence_encrypt(x, n)
  } else {
    x <- .railfence_decrypt(x, n)
  }

  return(x)
}

#' @keywords internal
.railfence_encrypt <- function(x, n) {
  # x is a list of split character vectors
  x <- strsplit(x, "")

  # loop over each item in the list of input vectors
  x <- lapply(x, function(y) {
    nx <- length(y)
    # create a numeric vector that goes and up down by integers
    z <- c(1, rep_len(c(2:n, (n - 1):1), nx - 1))
    # assign each letter to a position and order by position
    y <- data.frame("x" = y, "pos" = z)
    y <- y[order(y$pos), ]
    # collapse individual characters back into a string
    y <- paste0(y$x, collapse = "")
    return(y)
  })

  x <- unlist(x, recursive = FALSE)

  return(x)
}

#' @keywords internal
.railfence_decrypt <- function(x, n) {
  x <- lapply(x, function(s) {
    s <- unlist(strsplit(s, ""))
    # create a matrix and fill it with "*" where a character will be placed
    m <- matrix(NA, n, length(s))
    j <- 1
    dir <- 1
    for (i in 1:length(s)) {
      m[j, i] <- "*"

      if (dir == 1) {
        if (j == n) {
          dir <- -1
          j <- j - 1
        } else {
          j <- j + 1
        }
      } else {
        if (j == 1) {
          dir <- 1
          j <- j + 1
        } else {
          j <- j - 1
        }
      }
    }
    # loop over the matrix and fill characters where "*"
    k <- 1
    for (j in 1:nrow(m)) {
      for (i in 1:ncol(m)) {
        if (!is.na(m[j, i])) {
          m[j, i] <- s[k]
          k <- k + 1
        }
      }
    }
    # collapse individual characters back into a string
    m <- as.vector(m)
    m <- paste(m[!is.na(m)], collapse = "")

    return(m)
  })

  x <- unlist(x, recursive = FALSE)

  return(x)
}
