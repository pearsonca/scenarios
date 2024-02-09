
#' @title Quantile Summarizer
#'
#' @description
#' A summarizer for use in [scn_analyze()] for `summary` argument. Creates quantiles
#'
#' @param probs like the `probs` argument to [stats::quantile()], but supports names
#' The names will be used as the column names.
#'
#' @return a function, suitable for application to long-format data in grouped chunks
#'
#' @export
scn_quantile <- function(probs = c(lo = 0.025, md = 0.5, hi = 0.975)) {
  return(\(x) { quantile(x, probs = probs, na.rm = TRUE) |> setNames(names(probs)) |> as.list() })
}
