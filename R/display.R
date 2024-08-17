#' @export
format.maskr_masked <- function(
    x,
    ...,
    rep = getOption('maskr.replacement', 'n.p.')) {
  check_replacement(rep)

  data <- field(x, 'data')
  mask <- field(x, 'mask')
  na <- is.na(data)
  num <- is.numeric(data)

  args <- list(...)

  if (any(na)) {
    args$width <- max(args$width, 2L, 0L) # Width of 'NA'
  }
  if (any(mask)) {
    args$width <- max(args$width, nchar(rep), 0L)
  }

  # Initialise formatted vector
  fmt <- character(length(x))

  # Fill with non-missing, non-suppressed data
  args$x <- vec_slice(data, !mask & !na)
  vec_slice(fmt, !mask & !na) <- do.call('format', args)

  # For masked/missing, need to justify right for numerics
  if (num) args$justify <- 'right'

  # Ensure that width of not published/NA is correct
  if (is.null(args$justify) || args$justify != 'none') {
    args$width <- max(nchar(fmt), args$width, 0L, na.rm = TRUE)
  }

  # Fill with missing, non-masked data
  args$x <- 'NA'
  vec_slice(fmt, !mask & na) <- do.call('format', args)

  # Fill with masked data
  args$x <- rep
  vec_slice(fmt, mask) <- do.call('format', args)

  fmt
}

#' @export
#' @importFrom cli ansi_string col_red col_grey
obj_print_data.maskr_masked <- function(x, ...) {
  fmt <- ansi_string(format(x, ...))
  na <- is.na(unmask(x))
  masked <- mask(x)

  fmt[na] <- col_red(fmt[na])
  fmt[masked] <- col_grey(fmt[masked])

  cat(fmt, fill = TRUE)
  invisible(x)
}

#' @export
as.character.maskr_masked <- function(
    x,
    ...,
    rep = getOption('maskr.replacement', 'n.p.')) {
  check_replacement(rep)

  ret <- character(length(x))
  data <- field(x, 'data')
  mask <- field(x, 'mask')

  ret
  ret[mask] <- rep
  ret[!mask] <- as.character(data[!mask])

  ret
}

#' @importFrom rlang is_character
check_replacement <- function(rep) {
  if (length(rep) != 1L) {
    cli_abort(
      "Replacement {.var rep} must have length 1.",
      class = c('maskr_error_rep_length', 'maskr_error_replacement', 'maskr_error')
    )
  }

  if (!is_character(rep)) {
    cli_abort(
      message = c(
        "Replacement {.var rep} must be a character value.",
        i = "Instead, replacement is {.val {rep}} of type {.cls {class(rep)}}."
      ),
      class = c('maskr_error_rep_type', 'maskr_error_replacement', 'maskr_error')
    )
  }

  invisible(rep)
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.maskr_masked <- function(x, ..., rep = getOption('maskr.replacement', 'n.p.')) {
  check_replacement(rep)
  shft <- pillar::pillar_shaft(unmask(x), ...)
  class(shft) <- c("pillar_shaft_maskr_masked", class(shft))
  attr(shft, 'replacement') <- rep
  attr(shft, 'mask') <- mask(x)

  if (any(mask(x))) {
    attr(shft, 'min_width') <- max(attr(shft, 'min_width'), nchar(rep))
    attr(shft, 'width') <- max(attr(shft, 'width'), nchar(rep))
  }

  shft
}

#' @export
format.pillar_shaft_maskr_masked <- function(x, width, ...) {
  orn <- NextMethod('format')
  orn[attr(x, 'mask')] <- pillar::style_subtle(attr(x, 'replacement'))
  orn
}
