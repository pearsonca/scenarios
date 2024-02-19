
#' @title Extract Scenario Table
#'
#' @description
#' Extract scenario table from a full table of parameters, settings, etc.
#'
#' @param data a [data.table()] of parameter / settings
#'
#' @param exclude column names to exclude. defaults to [data.table::key()] of `dt`
#'
#' @param orderby for the output, what column priority to order on, which
#' determines the scenario labeling order. By default, the current order of the
#' non-`exclude`d columns of `data`
#'
#' @details
#' This method extracts the unique scenario set from a parameter table. It is
#' easiest to use on a data.table, that already has a key, where that key
#' is like a primary key in a relational data sense.
#'
#' Note: if your data already have something like a scenario table, you don't
#' need to use this method, but you should make sure that scenario table
#' conforms to the return value for this function: an `id` column, and all the
#' unique combinations of feature columns.
#'
#' @return a [data.table()] with an `id` column and where all other columns form
#' the unique combinations of the non-excluded columns in dt.
#'
#' @importFrom data.table key
#' @importFrom data.table setkeyv
#' @importFrom data.table setcolorder
#'
#' @examples
#' library(scenarios)
#' data(covid_abm_eg)
#' covid_abm_eg |> scn_extract(c("realization", "date", "inf", "deaths"))
#'
#' @export
scn_extract <- function(
  data,
  exclude = data.table::key(data),
  orderby = setdiff(names(data), exclude)
) {
  scen_dt <- data[, unique(.SD), .SDcols = -c(exclude)]
  setkeyv(scen_dt, orderby)
  scen_dt[, scen_id := seq_len(.N)]
  setkeyv(scen_dt, c("scen_id", key(scen_dt)))
  setcolorder(scen_dt)
  return(scen_dt[])
}

#' @title Convert Data to be Keyed by Scenario
#'
#' This method takes data which has existing scenario columns, and returns that
#' data converted to be in terms of the scenario keys.
#'
#' @param data a data.table, such as might be provided to [scn_extract()]
#'
#' @param scen_dt a data.table, meeting the return criteria of [scn_extract()]
#'
#' @param keep the columns to keep; by default, all of the columns that columns
#' that are *not* defining scenarios.
#'
#' @return a data.table, with columns `keep` + `scen_id`
#'
#' @export
scn_convert <- function(
  data, scen_dt, keep = setdiff(names(data), names(scen_dt))
) {
  scencols <- setdiff(names(scen_dt), "scen_id")
  data[scen_dt, on = scencols][, .SD, .SDcols = keep]
}

#' @title Create a Scenario Analysis Plan
#'
#' @description
#' A short description...
#'
#' @param scen_dt a [data.table::data.table()] as returned by [scn_extract()] and
#' passing [scn_check()]
#'
#' @param baseline creates a plan where features match, except those named
#' by this argument. The scenarios with the corresponding values for these names
#' become the reference values
#'
#' @param ignore other columns that can be ignored when matching `baseline`. The
#' `scen_id` column is always automatically ignored
#'
#' @return a data.table, with columns `comp`(arison) and `base`(line), both
#' integer id columns, corresponding to values from `scen_dt`
#'
#' @importFrom data.table rbindlist
#' @export
scn_plan <- function(
  scen_dt, baseline, ignore = c()
) {
  # these are the features to be used with `on = c(matchon)`
  matchon <- setdiff(names(scen_dt), c("scen_id", names(baseline), ignore))
  # create the filter
  reffilter <- (baseline |> lapply(\(v) list(pat = sprintf("(%%s == %s)" , v))) |> rbindlist(idcol = "col"))[,
    paste(sprintf(pat, col), collapse = " & ")
  ] |> parse(text = _)
  base_dt <- scen_dt[eval(reffilter), .SD, .SDcols = c("scen_id", matchon)]
  scen_dt[!eval(reffilter)][base_dt, on = c(matchon), .(comp = scen_id, base = i.scen_id)][]
}

#' @title title
#'
#' @return a reduced view of `data`, replacing all scenario information with a
#' `scen_id` column.
#'
#' @export
scn_outcomes <- function(
  data, scen_dt
) {
  scencols <- setdiff(names(scen_dt), "scen_id")
  data[scen_dt, on = scencols][, .SD, .SDcols = -scencols]
}

#' @title Execute Scenario Analysis
#'
#' @description
#' Conduct a scenario analysis according to a particular comparison plan, for
#' a set of given comparisons, optionally summarized.
#'
#' @param outcomes_dt a set of outputs, as produced by [scn_outcomes()]
#'
#' @importFrom data.table melt.data.table
#' @importFrom data.table setnames
#' @export
scn_analyze <- function(
  outcomes, plan,
  compare, summarize,
  inputs, group,
  joinon = setdiff(names(outcomes), inputs)
) {

  grid_dt <- (outcomes[
    plan, on = .(scen_id = comp), nomatch = 0
  ] |> setnames(c("scen_id", "base"), c("comp", "scen_id")))[
    outcomes, on = c(joinon), nomatch = 0
  ]

  grid_dt[, c(names(compare)) := lapply(compare, \(f) eval(f)), by = c(c("comp", "scen_id"), group)]
  res_dt <- grid_dt[, .SD, .SDcols = c(joinon, "comp", names(compare))] |> setnames("scen_id", "base")

  if (missing(summarize)) {
    return(res_dt)
  } else {
    res_dt <- res_dt |> melt.data.table(measure.vars = names(compare), variable.name = "measure")
    res_dt[, summarize(value), by = c(setdiff(joinon, c(group, "scen_id")), "base", "comp", "measure")]
  }

}
