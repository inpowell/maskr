test_that("suppression hides selected values", {
  expect_snapshot(suppressed(c(1L, 2L, NA, NA), c(FALSE, TRUE, FALSE, TRUE)))
  expect_snapshot(suppressed(c(1, 2, NA, NA), c(TRUE, FALSE, TRUE, FALSE)))
  expect_snapshot(suppressed(c('1', '2', NA, NA), c(TRUE, FALSE, FALSE, TRUE)))
})

test_that("formatted suppression has correct alignment", {
  num <- suppressed(c(12345, 12345678), c(FALSE, TRUE))
  obsnum <- fansi::strip_ctl(unclass(format(num)))
  expnum <- c("12345", " n.p.")
  expect_equal(obsnum, expnum)

  chr <- suppressed(c("abcde", "fghijkl"), c(FALSE, TRUE))
  obschr <- fansi::strip_ctl(unclass(format(chr)))
  expchr <- c('abcde', 'n.p. ')
  expect_equal(obschr, expchr)
})
