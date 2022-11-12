test_that("Encryption works", {
  expect_equal(running_key("testmessage", "thisisakeytothecipher"), "mlaluwsceex")
})

test_that("Decryption works", {
  expect_equal(running_key("mlaluwsceex", "thisisakeytothecipher", decrypt = TRUE), "testmessage")
})

test_that("Key must be longer than x", {
  expect_error(running_key("testmessage", "key"))
})
