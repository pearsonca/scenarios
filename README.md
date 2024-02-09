# Overview

`{scenarios}` simplifies scenario analysis by managing the bookkeeping work for you.

# Example

If you're doing scenario analysis, you've got some data that specify the scenario (i.e. effectively the inputs to your simulations) and some data corresponding to outputs. What you need to do is systematically compare those outputs, according to a plan to do with matching them up according to their inputs (e.g. compare the with-and-without treatment results, while holding all other varying inputs constant).

Perhaps you've just got everything in a single giant table. With `{scenarios}`, your analysis could be as simple as:

```r
library(scenarios)

# load data - this is a relatively large example simulation output
data(covid_abm_eg)

# extract the scenarios - provide your data + plus which columns are *not* inputs
scenario_dt <- scn_extract( # extract scenarios
  data = covid_abm_eg,
  exclude = c("realization", "date", "inf", "deaths")
)
scenario_dt

# create a scenario comparison plan - tell `scenarios` which column + values
# form the baseline, and it will match everything else up by default
plan_dt <- scn_plan(
  scenario_dt, baseline = list(pas_vac = 0, act_vac = 0), ignore = c("pas_alloc", "act_alloc", "inf_con")
)
plan_dt

# extract your results component & tag with which scenario it is
outcome_dt <- scn_outcomes(covid_abm_eg, scenario_dt)
outcome_dt

# execute the plan with a specified comparison, in this case c(umulative) eff(ectiveness), aggregated by quantiles.
ceff_dt <- scn_analyze(
  outcomes = outcome_dt, plan = plan_dt,
  compare = list(infc_ceff = scn_ceff("inf"), death_ceff = scn_ceff("deaths")),
  summary = scn_quantile(probs = c(q95l = 0.025, q50l = 0.25, md = 0.5, q50u = 0.75, q95u = 0.975))
  inputs = c("inf", "deaths"), group = "realization"
)
```

