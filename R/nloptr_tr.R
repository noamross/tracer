# use sink instead of capture output to allow printing to console as well, printing messages/errors
# sink("variable", append=FALSE, type="output", split=
#' @import stringi magrittr nloptr
#' @export
nloptr_tr = function( x0,
                      eval_f,
                      eval_grad_f = NULL,
                      lb = NULL,
                      ub = NULL,
                      eval_g_ineq = NULL,
                      eval_jac_g_ineq = NULL,
                      eval_g_eq = NULL,
                      eval_jac_g_eq = NULL,
                      opts = list(),
                      ...) {

  split = !is.null(opts$print_level) && opts$print_level != 0

  if(!is.null(opts$print_level) && opts$print_level %in% c(1,2)) {
    message("print_level options 1 and 2 in nloptr_tr default to 3.")
  }

  opts$print_level = 3

  printed_output <- NULL
  conn <- textConnection("printed_output", "w", local = TRUE)
  sink(conn, type="output", split=split)

  out <- nloptr( x0,
                 eval_f,
                 eval_grad_f,
                 lb,
                 ub,
                 eval_g_ineq,
                 eval_jac_g_ineq,
                 eval_g_eq,
                 eval_jac_g_eq,
                 opts,
                 ...)

  sink()

  trace = paste(printed_output, collapse = "\n")
  trace_data = list(
    eval = trace %>%
      stri_extract_all_regex("(?<=iteration\\:\\s)\\d+(?=\\n)", simplify=TRUE) %>%
      as.vector(),


    fval = trace %>%
      stri_extract_all_regex("(?<=f\\(x\\) = )[\\s\\-\\d\\,\\.]+", simplify=TRUE) %>%
      stri_trim_both(),

    xvals = trace %>%
      stri_extract_all_regex("(?<=x = ?\\()[\\s\\-\\d\\,\\.]+", simplify=TRUE) %>%
      stri_trim_both() %>%
      stri_split_regex("[\\s\\,]+", simplify=TRUE),

    gvals = trace %>%
      stri_extract_all_regex("(?<=g\\(x\\) = ?\\()[\\s\\-\\d\\,\\.]+", simplify=TRUE, omit_no_match = TRUE) %>%
      stri_trim_both() %>%
      stri_split_regex("[\\s\\,]+", simplify=TRUE)
  )

  trace_data = Filter(function(x) length(x) != 0, trace_data)

  if(!is.null(trace_data$xvals) && is.matrix(trace_data$xvals)) {
    colnames(trace_data$xvals) <- paste0("xval", seq_len(ncol(trace_data$xvals)))
  }

  if(!is.null(trace_data$gvals) && is.matrix(trace_data$xvals)) {
    colnames(trace_data$xvals) <- paste0("gval", seq_len(ncol(trace_data$xvals)))
  }

  trace_data = do.call(cbind, trace_data)
  trace_data = as.data.frame(apply(trace_data, 2, as.numeric))

  attr(out, "trace") <- trace_data
  class(out) <- c(class(out), "traced")
  return(out)
}

