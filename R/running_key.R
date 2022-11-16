#' Encrypt or decrypt a Running Key Vigenere Cipher
#'
#' @export
#'
#' @description This can be used to create (encrypt) and solve (decrypt) a
#'   Running Key Vigenere Cipher. A Vigenere cipher uses a table of alphabetic
#'   caesar shifts for one to twenty-six. The key is made to have an equal
#'   length to the text by adding the first letters of the text to the key. Each
#'   letter and corresponding key value determine the grid location to choose
#'   the obfuscated letter from.
#'
#'   The Running Key Cipher Wikipedia entry provides more information on the
#'   methods used: [https://en.wikipedia.org/wiki/Running_key_cipher](https://en.wikipedia.org/wiki/Running_key_cipher)
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
running_key <- function(x, key, decrypt = FALSE, keep_punctuation = FALSE) {
  if (length(x) != 1) {
    stop("Please provide a vector of length one for x")
  } else if (!is.atomic(x) & !is.character(x) & !is.matrix(x)) {
    stop("x must be a character vector.")
  }

  if (length(key) != 1) {
    stop("Please provide a vector of length one for key")
  } else if (!is.character(key) & (is.matrix(key) | !is.atomic(key))) {
    stop("key must be a character vector.")
  }

  y <- unlist(strsplit(x, ""))
  y <- y[grepl("[A-z]", y)]

  k <- unlist(strsplit(key, ""))
  k <- k[grepl("[A-z]", k)]

  if(length(k) >= length(y)) {
    x <- vigenere(x, paste0(k, collapse = ""), decrypt = decrypt, keep_punctuation = keep_punctuation)
    return(x)
  } else {
    stop(paste("The key must have an equal or greater number of letters to the text. The key has",
               length(k), "characters and the message has", length(y), "characters."))
  }
}
