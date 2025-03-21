% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tableSurvival.R
\name{riskTable}
\alias{riskTable}
\title{Table with survival events}
\usage{
riskTable(
  x,
  eventGap = NULL,
  header = c("estimate"),
  type = "gt",
  groupColumn = NULL,
  .options = list()
)
}
\arguments{
\item{x}{Result from estimateSingleEventSurvival or estimateCompetingRiskSurvival.}

\item{eventGap}{Event gap defining the times at which to report the risk table
information. Must be one of the eventGap inputs used for the estimation function.
If NULL, all available are reported.}

\item{header}{A vector containing which elements should go into the header.
Allowed are: cdm_name, group, strata, additional, variable, estimate,
and settings.}

\item{type}{Type of desired formatted table, possibilities: "gt",
"flextable",  and "tibble".}

\item{groupColumn}{Columns to use as group labels.}

\item{.options}{Named list with additional formatting options.
CohortSurvival::optionsTableSurvival() shows allowed arguments and their
default values.}
}
\value{
A tibble containing the risk table information (n_risk, n_events, n_censor)
for all times within the event gap specified.
}
\description{
Table with survival events
}
\examples{
\donttest{
cdm <- mockMGUS2cdm()
surv <- estimateSingleEventSurvival(cdm,
                                    targetCohortTable = "mgus_diagnosis",
                                    outcomeCohortTable = "death_cohort")
riskTable(surv)
}
}
