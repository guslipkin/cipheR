test_that("Punctuation is preserved for encryption", {
  expect_equal(vigenere("Test.message", "key", keep_punctuation = TRUE), "Diqd.qccwyqi")
  expect_equal(vigenere("testmessage", "key", keep_punctuation = FALSE), "diqdqccwyqi")
})
test_that("Punctuation is not preserved for encryption", {
  expect_equal(vigenere("Test.message", "key", keep_punctuation = FALSE), "diqdqccwyqi")
  expect_equal(vigenere("testmessage", "key", keep_punctuation = FALSE), "diqdqccwyqi")
})

test_that("Punctuation is preserved for decryption", {
  expect_equal(vigenere("Diqd.qccwyqi", "key", decrypt = TRUE, keep_punctuation = TRUE), "Test.message")
  expect_equal(vigenere("diqdqccwyqi", "key", decrypt = TRUE, keep_punctuation = TRUE), "testmessage")
})
test_that("Punctuation is not preserved for decryption", {
  expect_equal(vigenere("Diqd.qccwyqi", "key", decrypt = TRUE, keep_punctuation = FALSE), "testmessage")
  expect_equal(vigenere("diqdqccwyqi", "key", decrypt = TRUE, keep_punctuation = TRUE), "testmessage")
})

test_that("x must be a vector with length > 0", {
  expect_error(vigenere(c(), "key"))
  expect_error(vigenere(matrix(NA, 1, 1), "key"))
})
test_that("key must be a vector with length == 1", {
  expect_error(vigenere("testmessage", c()))
  expect_error(vigenere("testmessage", matrix(NA, 1, 1)))
})

test_that("decrypt must be a boolean", {
  expect_equal(vigenere("Test.message", "key", decrypt = FALSE, keep_punctuation = TRUE), "Diqd.qccwyqi")
  expect_equal(vigenere("Diqd.qccwyqi", "key", decrypt = TRUE, keep_punctuation = TRUE), "Test.message")
  expect_error(vigenere("testmessage", "key", decrypt = 1))
})

test_that("keep_punctuation must be a boolean", {
  expect_equal(vigenere("Test.message", "key", decrypt = FALSE, keep_punctuation = TRUE), "Diqd.qccwyqi")
  expect_equal(vigenere("Diqd.qccwyqi", "key", decrypt = TRUE, keep_punctuation = FALSE), "testmessage")
  expect_error(vigenere("testmessage", "key", keep_punctuation = 1))
})
