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

test_that("masked vectors have pretty printing in tibbles", {
  withr::local_package('tibble')
  withr::local_package('dplyr')
  withr::local_package('pillar')

  test_table <- tibble(
    Logical = rep_len(c(FALSE, TRUE), 5L),
    ShortInt = 8:12,
    Integer = 8:12 * 1234L,
    Double = 8:12 * 10 + 12:8 / 100,
    BigDouble = 64L^(5L*(-2:2)),
    Factor = gl(2, 1, 5, labels = c('A', 'B')),
    Ordered = gl(5, 1, 5, labels = c('X', 'Y', 'Z', 'U', 'V'), ordered = TRUE),
    Character = c('ABCDE', 'BCDEF', 'CDEFGHI', 'DEFGH', 'EFGHIJKL'),
    LongCharacter = paste(LETTERS, collapse = ' ')
  )

  test_table <- vctrs::vec_rbind(test_table, NA)

  for (idx in seq_along(test_table)) {
    mask <- rep(FALSE, 6L)
    mask[[6L - idx %% 6L]] <- TRUE
    test_table[[idx]] <- masked(test_table[[idx]], mask)
  }

  expect_snapshot(print(test_table, width = 100L))
  expect_snapshot(print(test_table, width =  90L))
  expect_snapshot(print(test_table, width =  80L))
  expect_snapshot(print(test_table, width =  60L))
  expect_snapshot(print(test_table, width =  40L))

  withr::with_options(
    list(maskr.replacement = '*'),
    expect_snapshot(print(test_table, width = 100L))
  )
})
