#' @export
tracer <- function(x) UseMethod("tracer")

tracer.default <- function(...) {
  stop("Object must be of class 'traced'")
  }

tracer.traced = function(opt) {
  attr(opt, "trace")
}