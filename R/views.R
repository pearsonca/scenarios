
#' @title Extract Baseline or Comparison Views
#'
#' @description
#' `{scenarios}` is primarily for preparing matched comparative analyses, but
#' you may also need to extract the uncompared views. The function let's you
#' use your analysis plan from [scn_plan()] to extract your baseline or
#' comparison scenarios.
#'
#' @param data a data.table, like returned from [scn_convert()] - i.e. having
#' a `scen_id` column, other outcome keying columns, and then outcome values
#'
#' @param plan a data.table, like returned from [scn_plan()] - i.e. having
#' `comp` and `base` columns that correspond to scenario ids.
#'
#' @param view a string, either "baseline" or "comparison", indicating which
#' view to return
#'
#' @return a filtered copy of `data`, with either the baseline scenario(s) or
#' the comparison ones, depending on `view` argument.
#'
#' @export
scn_view <- function(data, plan, view = c("baseline", "comparison")) {
  view <- match.arg(view)
  scen_ids <- switch (view,
    baseline = plan[, unique(base)],
    comparison = plan[, unique(comp)]
  )
  return(data[scen_id %in% scen_ids][])
}
