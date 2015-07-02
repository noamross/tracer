#' @export
tracer <- function(x) UseMethod("tracer")

#' @export
tracer.default <- function(...) {
  stop("Object must be of class 'traced'")
}

#' @export
tracer.traced = function(opt) {
  attr(opt, "trace")
}