test_that("encryption works", {
  expect_equal(railfence("abc def ghij", 3), "adgb e hjcfi")
  expect_equal(railfence(c("abc", "def", "ghij"), 2), c("acb", "dfe", "gihj"))
})
test_that("decryption works", {
  expect_equal(railfence("adgb e hjcfi", 3, decrypt = TRUE), "abc def ghij")
  expect_equal(railfence(c("acb", "dfe", "gihj"), 2, decrypt = TRUE),
               c("abc", "def", "ghij"))
})

test_that("must be a vector with length > 0", {
  expect_error(railfence(c(), 3))
  expect_error(railfence(matrix(NA, 1, 1), 3))
})

test_that("n must be a single integer greater than or equal to 1", {
  expect_error(railfence("abc", 0))
  expect_error(railfence("abc", 1.5))
  expect_error(railfence("abc", 0:3))
})

test_that("decrypt must be logical", {
  expect_error(railfence("abc", 3, "true"))
})
