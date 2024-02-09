
#' @title Cumulative Effectiveness Comparator
#'
#' @description
#' Convenience function to create a cumulative effectiveness comparator for use
#' in [scn_analyze()] for the `compare` argument.
#'
#' @param colname a string, the column name in the output that will be compared
#'
#' @return a expression, which will be evaluated in the context of the output
#'
#' @export
#'
#' @examples
#' # by default, assume a `melt.data.table`-like result, with `value` as the
#' # column
#' scn_ceff()
#'
#' # but if in wide-format, can instead give the column names
#' scn_ceff("infections")
#' scn_ceff("deaths")
scn_ceff <- function(colname = "value") { parse(text = sprintf("{
  # ci - the cumulative base value
  # ce - the cumulative compare value
  ci <- cumsum(i.%s); ce <- cumsum(%s)
  fifelse(ci == ce, 0, (ci - ce)/ci)
}", colname, colname)) }
