test_that("missing values aren't allowed in mask by default", {
  expect_error(masked(c(1, 2), c(NA, TRUE)), class = 'maskr_error_missing_mask')
})

test_that("masked getters and setters work", {
  data_pre <- 1:10
  mask_pre <- rep(c(FALSE, TRUE), times = 5L)

  msk <- masked(data_pre, mask_pre)
  expect_equal(unmask(msk), data_pre)
  expect_equal(mask(msk), mask_pre)

  data_post <- c(1:5, rep(NA, 5L))
  mask_post <- c(rep(c(FALSE, TRUE), times = 3L), rep(FALSE, 4L))
  unmask(msk)[6:10] <- NA
  mask(msk)[7:10] <- FALSE

  expect_equal(unmask(msk), data_post)
  expect_equal(mask(msk), mask_post)
})

test_that("mask setter can't create missing values", {
  data_pre <- 1:10
  mask_pre <- rep(c(FALSE, TRUE), times = 5L)

  msk <- masked(data_pre, mask_pre)

  expect_error(mask(msk)[1] <- NA, class = 'maskr_error_missing_mask')
})

test_that("coercion with masked vectors follows coercion hierarchy", {
  msk_lgl <- masked(logical())
  msk_int <- masked(integer())
  msk_dbl <- masked(double())
  msk_dte <- masked(as.Date(double()))
  msk_chr <- masked(character())
  msk_fct <- masked(factor(levels = 'a'))
  msk_ord <- masked(ordered(levels = 'a'))

  # See, e.g. https://vctrs.r-lib.org/reference/theory-faq-coercion.html#coercion-hierarchy

  # Logical
  expect_equal(vec_ptype2(msk_lgl, msk_int), msk_int)
  expect_equal(vec_ptype2(msk_lgl, msk_dbl), msk_dbl)
  expect_error(vec_ptype2(msk_lgl, msk_dte), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_lgl, msk_chr), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_lgl, msk_fct), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_lgl, msk_ord), class = 'vctrs_error_incompatible_type')

  # Integer
  expect_equal(vec_ptype2(msk_int, msk_dbl), msk_dbl)
  expect_error(vec_ptype2(msk_int, msk_dte), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_int, msk_chr), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_int, msk_fct), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_int, msk_ord), class = 'vctrs_error_incompatible_type')

  # Double
  expect_error(vec_ptype2(msk_dbl, msk_dte), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_dbl, msk_chr), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_dbl, msk_fct), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_dbl, msk_ord), class = 'vctrs_error_incompatible_type')

  # Date
  expect_error(vec_ptype2(msk_dte, msk_chr), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_dte, msk_fct), class = 'vctrs_error_incompatible_type')
  expect_error(vec_ptype2(msk_dte, msk_ord), class = 'vctrs_error_incompatible_type')

  # Character
  expect_equal(vec_ptype2(msk_fct, msk_chr), msk_chr)
  expect_equal(vec_ptype2(msk_ord, msk_chr), msk_chr)

  # Factor
  expect_error(vec_ptype2(msk_fct, msk_ord), class = 'vctrs_error_incompatible_type')
})

test_that("casting between masked vectors follows hierarchy", {
  msk_lgl <- masked(c(FALSE, TRUE), c(FALSE, TRUE))
  msk_int <- masked(c(0L, 1L),      c(FALSE, TRUE))
  msk_dbl <- masked(c(0, 1),        c(FALSE, TRUE))
  msk_chr <- masked(c('a', 'b'),    c(FALSE, TRUE))
  msk_fct <- masked(factor(c('a', 'b'), levels = c('a', 'b')),  c(FALSE, TRUE))
  msk_ord <- masked(ordered(c('a', 'b'), levels = c('a', 'b')), c(FALSE, TRUE))

  expect_equal(vec_cast(msk_lgl, msk_int[0]), msk_int)
  expect_equal(vec_cast(msk_lgl, msk_dbl[0]), msk_dbl)
  expect_equal(vec_cast(msk_int, msk_dbl[0]), msk_dbl)

  expect_equal(vec_cast(msk_fct, msk_chr[0]), msk_chr)
  expect_equal(vec_cast(msk_ord, msk_chr[0]), msk_chr)
})

test_that("casting from atomic vectors follows hierarchy", {
  msk_lgl <- masked(logical())
  msk_int <- masked(integer())
  msk_dbl <- masked(double())
  msk_dte <- masked(as.Date(double()))
  msk_chr <- masked(character())
  msk_fct <- masked(factor(levels = 'a'))
  msk_ord <- masked(ordered(levels = 'a'))

  expect_equal(vec_ptype2(msk_lgl, logical()), msk_lgl)
  expect_equal(vec_ptype2(logical(), msk_lgl), msk_lgl)
  expect_equal(vec_ptype2(msk_int, integer()), msk_int)
  expect_equal(vec_ptype2(integer(), msk_int), msk_int)
  expect_equal(vec_ptype2(msk_dbl, double()),  msk_dbl)
  expect_equal(vec_ptype2(double(), msk_dbl),  msk_dbl)

  # Casting up
  expect_equal(vec_ptype2(msk_lgl, double()), msk_dbl)
  expect_equal(vec_ptype2(double(), msk_lgl), msk_dbl)

  expect_equal(vec_ptype2(msk_dte, as.Date(double())), msk_dte)
  expect_equal(vec_ptype2(as.Date(double()), msk_dte), msk_dte)

  expect_equal(vec_ptype2(msk_chr, character()), msk_chr)
  expect_equal(vec_ptype2(character(), msk_chr), msk_chr)
  expect_equal(vec_ptype2(msk_fct, factor(levels = 'a')), msk_fct)
  expect_equal(vec_ptype2(factor(levels = 'a'), msk_fct), msk_fct)
  expect_equal(vec_ptype2(msk_ord, ordered(levels = 'a')), msk_ord)
  expect_equal(vec_ptype2(ordered(levels = 'a'), msk_ord), msk_ord)

  # Casting up
  expect_equal(vec_ptype2(msk_chr, factor()), msk_chr)
  expect_equal(vec_ptype2(msk_chr, ordered()), msk_chr)
  expect_equal(vec_ptype2(msk_fct, character()), msk_chr)
  expect_equal(vec_ptype2(msk_ord, character()), msk_chr)
})

test_that("coercion to masked types defaults to no masking", {
  letters_cast <- vec_cast(letters, masked(character()))
  expect_equal(unmask(letters_cast), letters)
  expect_equal(field(letters_cast, 'mask'), rep(FALSE, 26L))

  numbers_cast <- vec_cast(1:10, masked(integer()))
  expect_equal(unmask(numbers_cast), 1:10)
  expect_equal(field(numbers_cast, 'mask'), rep(FALSE, 10L))
})
