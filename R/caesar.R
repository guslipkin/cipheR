#' Encrypt or decrypt a Caesar Cipher
#'
#' @export
#'
#' @description This can be used to create (encrypt) and solve (decrypt) a
#'   Caesar cipher. The function does not differentiate between the two.
#'
#'   The Caesar Cipher Wikipedia entry provides more information on the methods
#'   used: [https://en.wikipedia.org/wiki/Caesar_cipher](https://en.wikipedia.org/wiki/Caesar_cipher)
#'
#' @param x A vector to be shifted
#' @param n (Default: `1`) The number of places to shift by. This can be either
#'   positive or negative. Zero returns x as it was given to the function.
#' @param preserve_spaces (Default: `TRUE`) A boolean describing if spaces should
#'   be preserved. This is helpful when working with sentences.
#' @param dict The dictionary used for shifting. This defaults to NULL in which
#'   case a dictionary is built from the sorted unique values of x.
#' @param preset A pre-made dictionary using ASCII codes from
#'   \url{https://www.ascii-code.com/}. Note that `delete` is excluded as a
#'   character.
#'   * `NULL` (the default)
#'   * `"alphanumeric"`: ASCII characters 48:57, 65:90, and 97:122. Numbers 0-9
#'   and both uppercase and lowercase letters from the English alphabet.
#'   * `"keyboard"`: ASCII characters 32:126. The characters you'll find on a
#'   US English keyboard.
#'   * `"letters"`: ASCII characters 65:90 and 97:122. Both uppercase and
#'   lowercase letters from the English alphabet.
#'   * `"lowercase"`: ASCII characters 97:122. Lowercase letters from the
#'   English alphabet.
#'   * `"uppercase"`: ASCII characters 65:90. Uppercase letters from the
#'   English alphabet.
#'
#' @returns A character vector of length one that has been shifted.
caesar <- function(x, n = 1, preserve_spaces = TRUE, dict = NULL, preset = NULL) {

  # Catching errors
  if (length(x) == 0) {
    stop("Please provide a vector of length greater than zero to shift.")
  } else if (!is.atomic(x) | !(is.character(x) | is.numeric(x))) {
    stop("x must be a numeric or character vector.")
  }

  if (!is.null(dict) & !is.null(preset)) {
    warning("Both a dict and a preset was provided, only the dict will be used.")
  }

  if (n == 0) {
    message("Shifting by zero doesn't do anything...")
    return(x)
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  x <- strsplit(x, "")
  unlistX <- unlist(x)

  # Set the dictionary as needed
  if (is.null(dict) & is.null(preset)) {
    dict <- sort(unique(unlistX))
  } else if (!is.null(preset)) {
    if (preset == "alphanumeric") {
      dict <- rawToChar(as.raw(c(48:57, 65:90, 97:122)))
    } else if (preset == "keyboard") {
      dict <- rawToChar(as.raw(32:126))
    } else if (preset == "letters") {
      dict <- rawToChar(as.raw(c(65:90, 97:122)))
    } else if (preset == "lowercase") {
      dict <- rawToChar(as.raw(c(97:122)))
    } else if (preset == "uppercase") {
      dict <- rawToChar(as.raw(c(65:90)))
    } else {
      stop("It looks like you may have a typo in your presets. You can double-check the presets by running ?caesar in your console.")
    }
  }

  if (length(dict) == 1) {
    dict <- unlist(strsplit(dict, ""))
  }

  if (!all(unlistX %in% dict)) {
    stop("Not all values of x are in the character set. Please choose a different character set.")
  }

  x <-
    lapply(x, function(y) {
      # We need to preserve spaces as requested
      if (preserve_spaces) {
        isSpace <- which(dict == " ")
        if (length(isSpace > 0)) {
          dict <- dict[-which(dict == " ")]
        }
      }

      # Do the shifting
      y <-
        sapply(y, function(z) {
          if (preserve_spaces & z == " ") {
            return(" ")
          }
          hop <- which(z == dict) + n
          hop <- hop %% length(dict)
          if (hop == 0) {
            hop <- length(dict)
          }
          return(dict[hop])
        })
      y <- paste0(y, collapse = "")
      return(y)
    })

  x <- unlist(x, recursive = FALSE)

  return(x)
}
