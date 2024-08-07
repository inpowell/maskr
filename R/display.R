#' @export
#' @importFrom glue as_glue
#' @importFrom cli ansi_string col_red col_grey
format.visnonp_suppressed <- function(
    x,
    ...,
    rep = getOption('visnonp.replacement', 'n.p.')) {
  data <- field(x, 'data')
  supp <- field(x, 'suppression')
  stopifnot(length(rep) == 1L)

  args <- list(...)
  args$x <- data

  if (any(supp)) {
    args$width <- max(args$width, nchar(rep)) # Width of 'n.p.'
  }

  fmt <- ansi_string(do.call('format', args))
  vec_slice(fmt, is.na(fmt)) <- col_red(vec_slice(fmt, is.na(fmt)))
  vec_slice(fmt, supp) <- col_grey(rep)

  fmt
}

#' @export
obj_print_data.visnonp_suppressed <- function(x, ...) {
  fmt <- format(x, ...)
  cat(fmt)
  invisible(x)
}
