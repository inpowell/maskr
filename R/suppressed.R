#' @importFrom rlang is_atomic
new_suppressed <- function(data = numeric(), suppression = logical()) {
  if (!is_atomic(data)) {
    cli::cli_abort(c(
      "{.var data} must be atomic.",
      'i' = "{.var data} has class {.cls {class(data)}}"
    ))
  }

  if (!is.logical(suppression)) {
    cli::cli_abort(c(
      "{.var pattern} must be a logical vector.",
      'i' = "{.var pattern} has class {.cls {class(pattern)}}"
    ))
  }

  new_rcrd(
    list(data = data, suppression = suppression),
    class = 'visnonp_suppressed'
  )
}

suppressed <- function(data = numeric(), suppression = logical()) {
  rec <- vec_recycle_common(data, suppression)
  new_suppressed(rec[[1]], rec[[2]])
}
