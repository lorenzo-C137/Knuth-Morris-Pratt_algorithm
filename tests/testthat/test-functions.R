test_that("basic test of KMP()", {
  text = 'AAATCGCTATG'
  pattern = 'ATC'
  expect_equal(KMP(pattern, text), 3)
})

test_that("test with empty string instead of P", {
  text = 'AAATCGCTATG'
  pattern = ''
  expect_error(KMP(pattern, text), "pattern is not of class 'character' or is an empty string")
})

test_that("test with number instead of S", {
  text = 1
  pattern = 'AAATCGCTATG'
  expect_error(KMP(pattern, text), "Text S is not of class 'character' or is an empty string")
})

test_that("test with a longer P than S", {
  text = 'AAA'
  pattern = 'AAATCGCTATG'
  expect_error(KMP(pattern, text), "Length of pattern P greater than length of text S .Choose a shorter pattern")
})

test_that("basic test of kmp_index()", {
  pattern = 'ATGCATA'
  expect_equal(kmp_index(pattern), c(-1, 0, 0, 0, -1, 0, 2, 1))
})
