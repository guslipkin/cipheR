test_that("Letters are flipped", {
  expect_equal(atbash("az"), "za")
})

test_that("Spaces are preserved", {
  expect_equal(atbash("a b c"), "z y x")
})
test_that("Punctuation is preserved", {
  expect_equal(atbash("a.b"), "z.y")
})

test_that("Vector length is preserved", {
  expect_equal(atbash(c("a", "b", "c")), c("z", "y", "x"))
})

test_that("Must be a character vector of length greater than zero", {
  expect_error(atbash(c(TRUE)))
  expect_error(atbash(c(123)))
  expect_error(atbash(c()))
})
