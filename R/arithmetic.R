#' @export
#' @method vec_arith maskr_masked
vec_arith.maskr_masked <- function(op, x, y, ...) {
  UseMethod("vec_arith.maskr_masked", y)
}

#' @export
#' @method vec_arith.maskr_masked maskr_masked
vec_arith.maskr_masked.maskr_masked <- function(op, x, y, ...) {
  d <- vec_arith(op, unmask(x), unmask(y), ...)
  m <- field(x, 'mask') | field(y, 'mask')
  new_masked(d, m)
}

#' @rawNamespace S3method(vec_arith.maskr_masked,logical,.vec_arith.maskr_masked.atomic)
#' @rawNamespace S3method(vec_arith.maskr_masked,numeric,.vec_arith.maskr_masked.atomic)
#' @rawNamespace S3method(vec_arith.maskr_masked,Date,.vec_arith.maskr_masked.atomic)
.vec_arith.maskr_masked.atomic <- function(op, x, y, ...) {
  d <- vec_arith(op, unmask(x), y, ...)
  new_masked(d, field(x, 'mask'))
}

#' @rawNamespace S3method(vec_arith.logical,maskr_masked,.vec_arith.atomic.maskr_masked)
#' @rawNamespace S3method(vec_arith.numeric,maskr_masked,.vec_arith.atomic.maskr_masked)
#' @rawNamespace S3method(vec_arith.Date,maskr_masked,.vec_arith.atomic.maskr_masked)
.vec_arith.atomic.maskr_masked <- function(op, x, y, ...) {
  d <- vec_arith(op, x, unmask(y), ...)
  new_masked(d, field(y, 'mask'))
}

#' @export
vec_math.maskr_masked <- function(.fn, .x, ...) {
  switch (.fn,
    # Summary functions mask their results if any input masked
    prod = ,
    sum = ,
    mean =
      new_masked(vec_math(.fn, as.double(unmask(.x)), ...), any(field(.x, 'mask'))),
    any = ,
    all =
      new_masked(vec_math(.fn, as.logical(unmask(.x)), ...), any(field(.x, 'mask'))),
    # Elementwise generics preserve mask
    abs = ,
    sign = ,
    sqrt = ,
    ceiling = ,
    floor = ,
    trunc = ,
    log = ,
    log10 = ,
    log2 = ,
    log1p = ,
    acos = ,
    acosh = ,
    asin = ,
    asinh = ,
    atan = ,
    atanh = ,
    exp = ,
    expm1 = ,
    cos = ,
    cosh = ,
    cospi = ,
    sin = ,
    sinh = ,
    sinpi = ,
    tan = ,
    tanh = ,
    tanpi = ,
    gamma = ,
    lgamma = ,
    digamma = ,
    trigamma = ,
    is.nan = ,
    is.finite = ,
    is.infinite =
      new_masked(vec_math(.fn, as.double(unmask(.x)), ...), field(.x, 'mask')),
    # Other operations (e.g. cumsum, cummean, etc) not supported
    cli_abort(
      "{.fun {.fn}.maskr_masked} is not supported",
      class = c('maskr_error_unsupported', 'maskr_error')
    )
  )
}
