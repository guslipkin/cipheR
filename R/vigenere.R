#' Encrypt or decrypt a Vigenere Cipher
#'
#' @export
#'
#' @description This can be used to create (encrypt) and solve (decrypt) a
#'   Vigenere Cipher. A Vigenere cipher uses a table of alphabetic caesar shifts
#'   for one to twenty-six. Each letter and corresponding key value determine
#'   the grid location to choose the obfuscated letter from.
#'
#'   The Vigenere Cipher Wikipedia entry provides more information on the methods
#'   used: [https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher)
#'
#' @param x A vector to be encoded or decoded.
#' @param key A character vector of length one to use as a key
#' @param decrypt (Default: `FALSE`) The default `FALSE` will encrypt while
#'   using `TRUE` will decrypt a given value of `x`.
#' @param keep_punctuation (Default: `FALSE`) The default `FALSE` will ignore
#'   case and punctuation and return a lowercase result. `TRUE` will match the
#'   input's case and punctuation.
#'
#' @returns A character vector of length equal to x that has been transformed
vigenere <- function(x, key, decrypt = FALSE, keep_punctuation = FALSE) {

  if (length(x) == 0) {
    stop("Please provide a vector of length greater than zero for x")
  } else if (!is.atomic(x) & !is.character(x) & !is.matrix(x)) {
    stop("x must be a character vector.")
  }

  if (length(key) != 1) {
    stop("Please provide a vector of length one for key")
  } else if (!is.character(key) & (is.matrix(key) | !is.atomic(key))) {
    stop("key must be a character vector.")
  }

  if (!is.logical(decrypt)) {
    stop("decrypt must be logical")
  }
  if (!is.logical(keep_punctuation)) {
    stop("decrypt must be logical")
  }

  square <-
    suppressWarnings(matrix(sapply(0:25, function(x) {
      caesar(letters, x)
    }), 26, 26))

  key <- tolower(key)

  if (!decrypt) {
    x <- .vigenere_encrypt(x, key, square, keep_punctuation)
  } else {
    x <- .vigenere_decrypt(x, key, square, keep_punctuation)
  }

  return(x)
}

#' @keywords internal
.vigenere_encrypt <- function(x, key, square, keep_punctuation) {
  x <- lapply(x, function(x) {
    x <- unlist(strsplit(x, ""))

    if (!keep_punctuation) { x <- x[grepl("[A-z]", x)] }
    lowerX <- tolower(x)

    key <- unlist(strsplit(key, ""))
    key <- .rep_key(x, key)

    r <- sapply(key, .get_letter)
    c <- sapply(lowerX, .get_letter)

    x <- sapply(1:length(x), function(i) {
      if (c[i] == 0) {
        y <- x[i]
        print(y)
      } else {
        y <- square[r[i], c[i]]
      }

      if (keep_punctuation & grepl("[A-Z]", x[i])) { return(toupper(y)) }
      else { return(y) }
    })

    x <- paste0(x, collapse = "")
    return(x)
  })
  x <- unlist(x, recursive = FALSE)
  return(x)
}

#' @keywords internal
.vigenere_decrypt <- function(x, key, square, keep_punctuation) {
  x <- lapply(x, function(x) {
    x <- unlist(strsplit(x, ""))

    if (!keep_punctuation) { x <- x[grepl("[A-z]", x)] }
    lowerX <- tolower(x)

    key <- unlist(strsplit(key, ""))
    key <- .rep_key(x, key)

    r <- sapply(key, .get_letter)

    x <- mapply(function(r, x, lowerX) {
      i <- square[1, which(square[r, ] == lowerX)]

      if (keep_punctuation & grepl("[A-Z]", x)) { i <- toupper(i) }
      else if (keep_punctuation & length(i) == 0) { i <- x }
      return(i)
    }, r, x, lowerX)

    x <- paste0(x, collapse = "")

    return(x)
  })
  x <- unlist(x, recursive = FALSE)
  return(x)
}

#' @keywords internal
.get_letter <- function(y) {
  y <- which(letters == y)
  if (length(y) == 0) { return(0) }
  else { return(y) }
}

#' @keywords internal
.rep_key <- function(x, key) {
  rep_key <- c()
  k <- 1
  return_key <- sapply(1:length(x), function(i) {
    ki <- k %% length(key)
    if (ki == 0) { ki <- length(key) }

    if (!grepl("[A-z]", x[i])) {
      rep_key <- c(rep_key, "")
    } else {
      rep_key <- c(rep_key, key[ki])
      k <<- k + 1
    }
    return(rep_key)
  })

  return(return_key)
}
