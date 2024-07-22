
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
#' This method extracts the unique scenario set from tabular data. It is
#' easiest to use on a `data.table`, that already has a key, where that key
#' is like a [primary key in a relational data sense](https://en.wikipedia.org/wiki/Primary_key).
#'
#' Note: if your data already have something like a scenario table, you don't
#' need to use this method, but you should make sure that scenario table
#' conforms to the return value for this function: `scen_id` column, and all the
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

#' @title Validate a Potential Scenario Table
#'
#' @param scen_dt a data.table, such as would be returned by [scn_extract()].
#' If the `scen_dt` does conform to the requirements for a scenario table,
#' throws an informative error.
#'
#' @return for convenience, returns `scen_dt` unmodified (if validated)
#'
#' @export
scn_validate <- function(scen_dt) {
  if (!is.data.table(scen_dt)) stop("`scen_dt` is not a data.table.")
  if (!("scen_id" %in% names(scen_dt))) stop("`scen_dt` does not have a `scen_id` column.")
  if (!scen_dt[, all(seq_len(.N ) == scen_id)]) stop("`scen_dt` is not a integer sequence from 1 to N, the number of scenarios.")
  if (scen_dt[, unique(.SD), .SDcols = -c("scen_id")][, .N] != scen_dt[, .N]) stop("the scenario columns are non-unique.")
  scen_dt
}

#' @title Convert Data to be Keyed by Scenario
#'
#' @description
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
  data[scen_dt, on = scencols][, .SD, .SDcols = c("scen_id", keep)]
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
#' `scen_id` column is always automatically ignored. This is useful when some of
#' your scenario features are irrelevant when running the baseline scenario
#' (e.g. vaccine efficacy, when no vaccine is distributed in the baseline case)
#' and therefore there are not any corresponding baseline runs with this feature
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
  res_dt <- scen_dt[!eval(reffilter)][base_dt, on = c(matchon), .(comp = scen_id, base = i.scen_id)]

  if (res_dt[, .N, by = comp][, any(N > 1)]) {
    stop("some comparison scenarios matched to multiple baselines")
  }

  if (res_dt[, unique(c(comp, base)) |> length()] != scen_dt$scen_id |> length()) {
    # TODO: more detailed warning if about which scenarios missing
    warning("incomplete scenario specification")
  }

  return(res_dt[])
}

#' @title Create an Analysis Grid
#'
#' @description
#' Creates a tabular view of outputs based on a comparison plan + matched output
#' features
#'
#' @export
scn_grid <- function(
  data, plan, match_by = key(data)
) {
  meascols <- setdiff(names(data), c("scen_id", match_by))
  colord <- c(match_by, "comp", "base", meascols, sprintf("i.%s", meascols))

  return((data[
    plan, on = .(scen_id = comp), nomatch = 0
  ] |> setnames(old = c("scen_id", "base"), new = c("comp", "scen_id")))[
    data, on = c(match_by, "scen_id"), nomatch = 0
  ] |> setnames("scen_id", "base") |> setcolorder(colord))

}

#' @title Execute Scenario Analysis
#'
#' @description
#' Conduct a scenario analysis according to a particular comparison plan, for
#' a set of given comparisons, optionally summarized.
#'
#' @param outcomes a set of outputs, as produced by [scn_outcomes()]
#'
#' @param plan pairwise scenario ids, as produced by [scn_plan()]: the
#' comparison plan
#'
#' @param compare optional, a list; names correspond to columns that will be
#' created, and the elements should be expressions to evaluate. `compare`
#' operations should be vectorized, but can use aggregating elements, and will
#' be applied by scenario pairs and any `group` argument.
#'
#' @param summarize optional, a list; names correspond to columns that will be
#' created and the elements should be expressions to evaluate. The operations
#' should yield scalar results per group. This will be evaluated after `compare`
#' so any columns it creates will be available.
#'
#' @param group when performing compare or summarize operations, how to group
#' those operations for any group-wise calculations. Operations are always
#' performed by scenario id pairs, and this parameter is for additional grouping
#'
#' @return a data.table
#'
#' @importFrom data.table melt.data.table
#' @importFrom data.table setnames
scn_analyze <- function(
  outcomes, plan,
  compare, summarize,
  group = list(compare = c(), summarize = c()),
  joinon = key(outcomes)
) {

  if (missing(compare) & missing(summarize)) stop("must provide at least one of `compare` and `summarize`.")

  grid_dt <- (outcomes[
    plan, on = .(scen_id = comp), nomatch = 0
  ] |> setnames(old = c("scen_id", "base"), new = c("comp", "scen_id")))[
    outcomes, on = c(joinon, "scen_id"), nomatch = 0
  ] |> setnames("scen_id", "base")

  if (!missing(compare)) {
    grid_dt[,
      c(names(compare)) := lapply(compare, \(f) eval(f)),
      by = c(c("comp", "base"), group$compare)
    ]
  }

  if (missing(summarize)) {
    return(grid_dt[, .SD, .SDcols = c(key(outcomes), "comp", "base", names(compare))])
  } else {
    grid_dt[,
      c(names(summarize)) := lapply(summarize, \(f) eval(f)),
      by = c(c("comp", "base"), group$summarize)
    ]
  }

}
