test_that("arithmetic on data works for logic/integer/numeric", {
  msk_lgl <- masked(TRUE, TRUE)
  msk_int <- masked(1L, TRUE)
  msk_dbl <- masked(1, TRUE)
  lgl <- TRUE
  int <- 1L
  dbl <- 1

  # masked/masked
  expect_equal(msk_lgl + msk_lgl, masked(2L, TRUE))
  expect_equal(msk_lgl + msk_int, masked(2L, TRUE))
  expect_equal(msk_lgl + msk_dbl, masked(2, TRUE))
  expect_equal(msk_int + msk_lgl, masked(2L, TRUE))
  expect_equal(msk_int + msk_int, masked(2L, TRUE))
  expect_equal(msk_int + msk_dbl, masked(2, TRUE))
  expect_equal(msk_dbl + msk_lgl, masked(2, TRUE))
  expect_equal(msk_dbl + msk_int, masked(2, TRUE))
  expect_equal(msk_dbl + msk_dbl, masked(2, TRUE))

  # masked/atomic
  expect_equal(msk_lgl + lgl, masked(2L, TRUE))
  expect_equal(msk_lgl + int, masked(2L, TRUE))
  expect_equal(msk_lgl + dbl, masked(2, TRUE))
  expect_equal(msk_int + lgl, masked(2L, TRUE))
  expect_equal(msk_int + int, masked(2L, TRUE))
  expect_equal(msk_int + dbl, masked(2, TRUE))
  expect_equal(msk_dbl + lgl, masked(2, TRUE))
  expect_equal(msk_dbl + int, masked(2, TRUE))
  expect_equal(msk_dbl + dbl, masked(2, TRUE))

  # atomic/masked
  expect_equal(lgl + msk_lgl, masked(2L, TRUE))
  expect_equal(lgl + msk_int, masked(2L, TRUE))
  expect_equal(lgl + msk_dbl, masked(2, TRUE))
  expect_equal(int + msk_lgl, masked(2L, TRUE))
  expect_equal(int + msk_int, masked(2L, TRUE))
  expect_equal(int + msk_dbl, masked(2, TRUE))
  expect_equal(dbl + msk_lgl, masked(2, TRUE))
  expect_equal(dbl + msk_int, masked(2, TRUE))
  expect_equal(dbl + msk_dbl, masked(2, TRUE))
})

test_that("mask flags are sticky on arithmetic", {
  m1 <- masked(1:4, c(FALSE, FALSE, TRUE, TRUE))
  m2 <- masked(5:8, c(FALSE, TRUE, FALSE, TRUE))

  expect_equal(field(m1+m2, 'mask'), c(FALSE, TRUE, TRUE, TRUE))

  expect_equal(m1 + 1, masked(2:5, c(FALSE, FALSE, TRUE, TRUE)))
  expect_equal(1 + m2, masked(6:9, c(FALSE, TRUE, FALSE, TRUE)))
})

test_that("mask flags are sticky on arithmetic", {
  m1 <- masked(1:4, c(FALSE, FALSE, TRUE, TRUE))
  m2 <- masked(5:8, c(FALSE, TRUE, FALSE, TRUE))

  expect_equal(field(m1+m2, 'mask'), c(FALSE, TRUE, TRUE, TRUE))

  expect_equal(m1 + 1, masked(2:5, c(FALSE, FALSE, TRUE, TRUE)))
  expect_equal(1 + m2, masked(6:9, c(FALSE, TRUE, FALSE, TRUE)))
})

test_that("summary math functions combine masks", {
  data <- 0:10
  mask <- c(TRUE, rep(FALSE, 10L))
  m <- masked(data, mask)
  u <- masked(data, FALSE)

  expect_equal(mean(m), masked(5, TRUE))
  expect_equal(mean(u), masked(5, FALSE))
  expect_equal(sum(m), masked(55, TRUE))
  expect_equal(sum(u), masked(55, FALSE))
})
