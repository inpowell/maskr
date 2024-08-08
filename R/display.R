#' @export
#' @importFrom cli ansi_string col_red col_grey
format.maskr_masked <- function(
    x,
    ...,
    rep = getOption('maskr.replacement', 'n.p.')) {
  stopifnot(length(rep) == 1L)

  data <- field(x, 'data')
  mask <- field(x, 'mask')
  na <- is.na(data)
  num <- is.numeric(data)

  args <- list(...)

  if (any(na)) {
    args$width <- max(args$width, 2L) # Width of 'NA'
  }
  if (any(mask)) {
    args$width <- max(args$width, nchar(rep))
  }

  # Initialise formatted vector
  fmt <- ansi_string(character(length(x)))

  # Fill with non-missing, non-suppressed data
  args$x <- vec_slice(data, !mask & !na)
  vec_slice(fmt, !mask & !na) <- ansi_string(do.call('format', args))

  # For masked/missing, need to justify right for numerics
  if (num) args$justify <- 'right'

  # Ensure that width of not published/NA is correct
  if (is.null(args$justify) || args$justify != 'none') {
    args$width <- max(nchar(fmt), args$width, na.rm = TRUE)
  }

  # Fill with missing, non-masked data
  args$x <- 'NA'
  vec_slice(fmt, !mask & na) <- col_red(do.call('format', args))

  # Fill with masked data
  args$x <- rep
  vec_slice(fmt, mask) <- col_grey(do.call('format', args))

  fmt
}

#' @export
obj_print_data.maskr_masked <- function(x, ...) {
  fmt <- format(x, ...)
  cat(fmt)
  invisible(x)
}
