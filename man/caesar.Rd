% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caesar.R
\name{caesar}
\alias{caesar}
\title{Encrypt or decrypt a Caesar Cipher}
\usage{
caesar(x, n = 1, preserve_spaces = TRUE, dict = NULL, preset = NULL)
}
\arguments{
\item{x}{A vector to be shifted}

\item{n}{(Default: \code{1}) The number of places to shift by. This can be either
positive or negative. Zero returns x as it was given to the function.}

\item{preserve_spaces}{(Default: \code{TRUE}) A boolean describing if spaces should
be preserved. This is helpful when working with sentences.}

\item{dict}{The dictionary used for shifting. This defaults to NULL in which
case a dictionary is built from the sorted unique values of x.}

\item{preset}{A pre-made dictionary using ASCII codes from
\url{https://www.ascii-code.com/}. Note that \code{delete} is excluded as a
character.
\itemize{
\item \code{NULL} (the default)
\item \code{"alphanumeric"}: ASCII characters 48:57, 65:90, and 97:122. Numbers 0-9
and both uppercase and lowercase letters from the English alphabet.
\item \code{"keyboard"}: ASCII characters 32:126. The characters you'll find on a
US English keyboard.
\item \code{"letters"}: ASCII characters 65:90 and 97:122. Both uppercase and
lowercase letters from the English alphabet.
\item \code{"lowercase"}: ASCII characters 97:122. Lowercase letters from the
English alphabet.
\item \code{"uppercase"}: ASCII characters 65:90. Uppercase letters from the
English alphabet.
}}
}
\value{
A character vector of length one that has been shifted.
}
\description{
This can be used to create (encrypt) and solve (decrypt) a
Caesar cipher. The function does not differentiate between the two.

The Caesar Cipher Wikipedia entry provides more information on the methods
used: \url{https://en.wikipedia.org/wiki/Caesar_cipher}
}
\examples{
(e1 <- caesar("abcde", 1))
caesar(e1, -1)

(e2 <- caesar("cipheR is a great R package!", -5))
caesar(e2, 5)

(e3 <- caesar("Isn't this fun?", 2, preserve_spaces = FALSE))
caesar(e3, -2, preserve_spaces = FALSE)
}
