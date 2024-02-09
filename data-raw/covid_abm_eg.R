
if (!requireNamespace("usethis")) stop("Need `usethis` to build data.")
if (!requireNamespace("RSQLite")) stop("Need `RSQLite` to build `covid_abm_eg.")

library(data.table)
library(RSQLite)

## also requires covid-abm data repo

## make/command-line-based approach
.args <- if (interactive()) c(
  file.path("..", "covid-abm", "exp", "active-vac", "covid-active-v7.sqlite"),
  file.path("data", "covid_abm_eg.rds")
) else commandArgs(trailingOnly = TRUE)

## extract from .args what we'd like to call this data
nm <- tail(.args, 1) |> basename() |> tools::file_path_sans_ext()

## get all the time series outputs + scenario inputs
assign(nm, {
  conn <- dbConnect(SQLite(), .args[1])
  dt <- conn |> dbGetQuery(
    "SELECT P.*, date, inf, deaths FROM meta JOIN par AS P USING(serial) WHERE realization < 10;"
  ) |> as.data.table()
  dbDisconnect(conn)
  dt[, .SD, .SDcols = -patterns("serial|seed")]
})

## store result in the correct place
eval(bquote(usethis::use_data(.(as.symbol(nm)), overwrite = TRUE)))
