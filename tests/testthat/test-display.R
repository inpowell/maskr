test_that("mask hides selected values", {
  expect_snapshot(masked(c(1L, 2L, NA, NA), c(FALSE, TRUE, FALSE, TRUE)))
  expect_snapshot(masked(c(1, 2, NA, NA), c(TRUE, FALSE, TRUE, FALSE)))
  expect_snapshot(masked(c('1', '2', NA, NA), c(TRUE, FALSE, FALSE, TRUE)))
})

test_that("formatted masks have correct alignment", {
  num <- masked(c(12345, 12345678), c(FALSE, TRUE))
  obsnum <- fansi::strip_ctl(unclass(format(num)))
  expnum <- c("12345", " n.p.")
  expect_equal(obsnum, expnum)

  chr <- masked(c("abcde", "fghijkl"), c(FALSE, TRUE))
  obschr <- fansi::strip_ctl(unclass(format(chr)))
  expchr <- c('abcde', 'n.p. ')
  expect_equal(obschr, expchr)
})

test_that("formatting zero-length masked vectors works", {
  msk <- masked()
  fmt <- format(msk)
  expect_equal(unclass(fmt), character())
})

test_that('as.character does not reveal masked data', {
  msk1 <- masked(c('abcde', 'defgh'), c(FALSE, TRUE))
  expect_equal(as.character(msk1), c('abcde', 'n.p.'))

  msk2 <- masked(c(1L, 2L), c(TRUE, FALSE))
  expect_equal(as.character(msk2), c('n.p.', '2'))
})

test_that("replacements must be length-1 character vectors", {
  msk <- masked(c('abcde', 'defgh'), c(FALSE, TRUE))
  expect_error(format(msk, rep = c('a', 'b')), class = 'maskr_error_rep_length')
  expect_error(as.character(msk, rep = c('a', 'b')), class = 'maskr_error_rep_length')
  expect_error(pillar::pillar_shaft(msk, rep = c('a', 'b')), class = 'maskr_error_rep_length')

  expect_error(format(msk, rep = 1L), class = 'maskr_error_rep_type')
  expect_error(as.character(msk, rep = FALSE), class = 'maskr_error_rep_type')
  expect_error(pillar::pillar_shaft(msk, rep = charToRaw('*')), class = 'maskr_error_rep_type')
})
