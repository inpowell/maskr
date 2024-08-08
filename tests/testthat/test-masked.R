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
