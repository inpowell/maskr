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

masked <- function(data = numeric(), mask = logical()) {
  rec <- vec_recycle_common(data, mask)
  new_masked(rec[[1]], rec[[2]])
}
