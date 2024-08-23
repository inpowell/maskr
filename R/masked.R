#' Create a masked atomic vector
#'
#' Masked vectors contain a base R data type that can be used in calculations,
#' but is not revealed by default on the console or when converted to a
#' character. This can be useful for preventing publication of small cells, such
#' as in official statistics.
#'
#' @section Pretty printing and conversion to character:
#'
#'   Masked vectors have pretty printing that replaces masked values of a vector
#'   with `n.p.` in the console (customisable with `options(maskr.replacement =
#'   ...)`).
#'
#'   Converting a masked vector to character results in masked vectors being
#'   replaced with `n.p.`, or its alternative in
#'   `getOption('maskr.replacement')`.
#'
#'   Masked vectors cannot be converted to their raw types, to prevent
#'   accidental release of data. Instead, use [unmask()] to explicitly unmask a
#'   vector.
#'
#' @section Arithmetic and unary mathematical functions:
#'
#'   Elementwise arithmetic operators have been implemented in masked vectors.
#'   The resulting data will be as if performing the operation on the unmasked
#'   vectors. Mask flags are sticky; the result of any operation involving a
#'   masked value will also be masked.
#'
#'   Most [Math] and [Summary] group generics have been implemented. These first
#'   force the underlying data to double type (or logical for `any()` and
#'   `all()`). Results from summary functions will be masked if any input is
#'   masked, while elementwise operations will preserve the mask from their
#'   input. Cumulative functions (`cumsum()`, `cummean()` and friends) are not
#'   implemented.
#'
#' @param data An atomic vector to mask values from. Lists and data frames are
#'   not supported.
#' @param mask A logical vector that indicates which values of `data` to mask.
#' @param masked A masked vector to extract fields from.
#' @param value A replacement vector for unmasked data, or the mask flags.
#'
#' @return A masked vector.
#' @export
#'
#' @examples
#' # Mask vowels in the alphabet
#' masked(letters, letters %in% c('a', 'e', 'i', 'o', 'u'))
#'
#' # Mask doubles...
#' sepals <- head(masked(iris$Sepal.Length, iris$Sepal.Length > 5))
#' sepals
#' # ...and get back the underlying values
#' unmask(sepals)
#'
#' # Use a different mask character
#' op <- options(maskr.replacement = '*')
#' sepals
#' options(op)
#'
#' # Mask integers in a vector strictly between 0 and 5
#' x <- 0:8
#' xm <- masked(x, 0L < x & x < 5L)
#'
#' # Arithmetic with unmasked values does not change mask:
#' xm / xm[8] * 100
#' 2 ^ xm
#' # But arithmetic with a masked value will mask outputs:
#' xm + xm[3]
#' xm - rev(xm)
#'
#' # The mean will be masked, because at least one if its inputs is masked
#' mean(xm)
#' unmask(mean(xm))
#'
#' # Other mathematical functions will keep the same mask
#' log1p(xm)
#' unmask(log1p(xm))
masked <- function(data = numeric(), mask = logical()) {
  rec <- vec_recycle_common(data, mask)

  if (any(is.na(mask))) {
    cli_abort(
      "{.var mask} must not contain missing values.",
      class = c('maskr_error_missing_mask', 'maskr_error')
    )
  }

  new_masked(rec[[1]], rec[[2]])
}

#' @export
#' @rdname masked
unmask <- function(masked) {
  if (!inherits(masked, 'maskr_masked')) {
    cli_abort(
      '{.fun unmask} needs a {.cls masked} vector from the {.pkg maskr} package.',
      class = c('maskr_error')
    )
  }
  field(masked, 'data')
}
#' @export
#' @rdname masked
`unmask<-` <- function(masked, value) {
  value <- vec_recycle(value, vec_size(masked))
  `field<-`(masked, 'data', value)
}

#' @export
#' @rdname masked
mask <- function(masked) {
  if (!inherits(masked, 'maskr_masked')) {
    cli_abort(
      '{.fun mask} needs a {.cls masked} vector from the {.pkg maskr} package.',
      class = c('maskr_error')
    )
  }
  field(masked, 'mask')
}
#' @export
#' @rdname masked
`mask<-` <- function(masked, value) {
  if (any(is.na(value))) {
    cli_abort(
      "Missing values nust not be set in {.fun mask}.",
      class = c('maskr_error_missing_mask', 'maskr_error')
    )
  }

  value <- vec_recycle(value, vec_size(masked))
  `field<-`(masked, 'mask', value)
}


#' @importFrom rlang is_atomic
new_masked <- function(data = numeric(), mask = logical()) {
  if (!is_atomic(data)) {
    cli::cli_abort(c(
      "{.var data} must be atomic.",
      'i' = "{.var data} has class {.cls {class(data)}}"
    ))
  }

  if (!is.logical(mask)) {
    cli::cli_abort(c(
      "{.var pattern} must be a logical vector.",
      'i' = "{.var pattern} has class {.cls {class(pattern)}}"
    ))
  }

  new_rcrd(
    list(data = data, mask = mask),
    class = 'maskr_masked'
  )
}

#' @export
vec_ptype_full.maskr_masked <- function(x, ...) {
  data <- field(x, 'data')
  paste0(vec_ptype_full(data, ...), '+masked')
}

#' @export
vec_ptype_abbr.maskr_masked <- function(x, ...) {
  data <- field(x, 'data')
  paste0(vec_ptype_abbr(data, ...), '+msk')
}

# Coercion and casting ----------------------------------------------------

#' @export
vec_ptype2.maskr_masked.maskr_masked <- function(x, y, ...) {
  xd <- field(x, 'data')
  yd <- field(y, 'data')
  new_masked(vec_ptype2(xd, yd, ...))
}

#' @export
vec_cast.maskr_masked.maskr_masked <- function(x, to, ...) {
  xdata <- field(x, 'data')
  xcast <- vec_cast(xdata, field(to, 'data'))
  new_masked(xcast, field(x, 'mask'))
}

#' @rawNamespace S3method(vec_ptype2,maskr_masked.logical,.vec_ptype2.maskr_masked.atomic)
#' @rawNamespace S3method(vec_ptype2,maskr_masked.integer,.vec_ptype2.maskr_masked.atomic)
#' @rawNamespace S3method(vec_ptype2,maskr_masked.double,.vec_ptype2.maskr_masked.atomic)
#' @rawNamespace S3method(vec_ptype2,maskr_masked.Date,.vec_ptype2.maskr_masked.atomic)
#' @rawNamespace S3method(vec_ptype2,maskr_masked.factor,.vec_ptype2.maskr_masked.atomic)
#' @rawNamespace S3method(vec_ptype2,maskr_masked.character,.vec_ptype2.maskr_masked.atomic)
#' @rawNamespace S3method(vec_ptype2,maskr_masked.ordered,.vec_ptype2.maskr_masked.atomic)
.vec_ptype2.maskr_masked.atomic <- function(x, y, ...) {
  xd <- field(x, 'data')
  new_masked(vec_ptype2(xd, y))
}

#' @rawNamespace S3method(vec_ptype2,logical.maskr_masked,.vec_ptype2.atomic.maskr_masked)
#' @rawNamespace S3method(vec_ptype2,integer.maskr_masked,.vec_ptype2.atomic.maskr_masked)
#' @rawNamespace S3method(vec_ptype2,double.maskr_masked,.vec_ptype2.atomic.maskr_masked)
#' @rawNamespace S3method(vec_ptype2,Date.maskr_masked,.vec_ptype2.atomic.maskr_masked)
#' @rawNamespace S3method(vec_ptype2,factor.maskr_masked,.vec_ptype2.atomic.maskr_masked)
#' @rawNamespace S3method(vec_ptype2,character.maskr_masked,.vec_ptype2.atomic.maskr_masked)
#' @rawNamespace S3method(vec_ptype2,ordered.maskr_masked,.vec_ptype2.atomic.maskr_masked)
.vec_ptype2.atomic.maskr_masked <- function(x, y, ...) {
  yd <- field(y, 'data')
  new_masked(vec_ptype2(x, yd))
}

#' @rawNamespace S3method(vec_cast,maskr_masked.logical,.vec_cast.maskr_masked.atomic)
#' @rawNamespace S3method(vec_cast,maskr_masked.integer,.vec_cast.maskr_masked.atomic)
#' @rawNamespace S3method(vec_cast,maskr_masked.double,.vec_cast.maskr_masked.atomic)
#' @rawNamespace S3method(vec_cast,maskr_masked.Date,.vec_cast.maskr_masked.atomic)
#' @rawNamespace S3method(vec_cast,maskr_masked.factor,.vec_cast.maskr_masked.atomic)
#' @rawNamespace S3method(vec_cast,maskr_masked.character,.vec_cast.maskr_masked.atomic)
#' @rawNamespace S3method(vec_cast,maskr_masked.ordered,.vec_cast.maskr_masked.atomic)
#' @importFrom rlang rep_along
.vec_cast.maskr_masked.atomic <- function(x, to, ...) {
  xcast <- vec_cast(x, field(to, 'data'))
  new_masked(xcast, rep_along(xcast, FALSE))
}
