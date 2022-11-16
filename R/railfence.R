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

  if (!decrypt) {
    x <- .railfence_encrypt(x, n)
  } else {
    x <- .railfence_decrypt(x, n)
  }

  return(x)
}

#' @keywords internal
.railfence_encrypt <- function(x, n) {
  x <- strsplit(x, "")

  x <- lapply(x, function(y) {
    nx <- length(y)
    z <- c(1, rep_len(c(2:n, (n - 1):1), nx - 1))
    y <- data.frame("x" = y, "pos" = z)
    y <- y[order(y$pos), ]
    y <- paste0(y$x, collapse = "")
    return(y)
  })

  x <- unlist(x, recursive = FALSE)

  return(x)
}

#' @keywords internal
.railfence_decrypt <- function(x, n) {
  x <-
    lapply(x, function(s) {
      s <- unlist(strsplit(s, ""))
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
      k <- 1
      for (j in 1:nrow(m)) {
        for (i in 1:ncol(m)) {
          if (!is.na(m[j, i])) {
            m[j, i] <- s[k]
            k <- k + 1
          }
        }
      }
      m <- as.vector(m)
      m <- paste(m[!is.na(m)], collapse = "")

      return(m)
    })

  x <- unlist(x, recursive = FALSE)

  return(x)
}
