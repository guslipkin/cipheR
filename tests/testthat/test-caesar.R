test_that("Shift 0 warns and works", {
  expect_message(caesar("abcde", 0))
  expect_equal(caesar("abcde", 0), "abcde")
})
test_that("Shift 1 works", {
  expect_equal(caesar("abcde", 1), "bcdea")
})
test_that("Shift -1 works", {
  expect_equal(caesar("abcde", -1), "eabcd")
})

test_that("Spaces are preserved", {
  expect_equal(caesar("a b c", 1), "b c a")
})
test_that("Spaces are not preserved", {
  expect_equal(caesar("a b c", 1, preserve_spaces = FALSE), "baca ")
})

test_that("Vector length is preserved", {
  expect_equal(caesar(c("a", "b", "c"), 1), c("b", "c", "a"))
})

test_that("When no dictionary is provided, x is used", {
  expect_equal(caesar("abcde", 1), "bcdea")
})

test_that("Cannot provide both a dict and a preset", {
  expect_warning(caesar("abcde", 1, dict = "abcde", preset = "letters"))
})

test_that("The alphanumeric preset works", {
  expect_equal(caesar("abc123", 1, preset = "alphanumeric"), "bcd234")
})
test_that("The keyboard preset works", {
  expect_equal(caesar("abc123!@#", 1, preset = "keyboard"), "bcd234\"A$")
})
test_that("The letters preset works", {
  expect_equal(caesar("abcABC", 1, preset = "letters"), "bcdBCD")
})
test_that("The lowercase preset works", {
  expect_equal(caesar("abcde", 1, preset = "lowercase"), "bcdef")
})
test_that("The uppercase preset works", {
  expect_equal(caesar("ABCDE", 1, preset = "uppercase"), "BCDEF")
})
test_that("A typo in the preset gives an error", {
  expect_error(caesar("abc", 1, preset = "abc"))
})

test_that("dict must contain all values in x", {
  expect_error(caesar("abc", 1, dict = "def"))
})

test_that("x cannot be empty", {
  expect_error(caesar(c(), 1))
})
test_that("x must be a vector", {
  expect_error(caesar(list("abc"), 1))
})
test_that("x must be a character or numeric vector", {
  expect_error(caesar(c(TRUE), 1))
})

