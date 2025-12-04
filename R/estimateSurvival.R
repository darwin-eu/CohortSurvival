# Copyright 2023 DARWIN EU®
#
# This file is part of CohortSurvival
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Estimate survival for a given event of interest using cohorts in the OMOP Common Data Model
#'
#' @param cdm CDM reference
#' @param targetCohortTable targetCohortTable
#' @param outcomeCohortTable The outcome cohort table of interest.
#' @param targetCohortId Target cohorts to include. It can either be a
#' cohort_definition_id value or a cohort_name. Multiple ids are allowed.
#' @param outcomeCohortId Outcome cohorts to include. It can either be a
#' cohort_definition_id value or a cohort_name. Multiple ids are allowed.
#' @param outcomeDateVariable  Variable containing date of outcome event
#' @param outcomeWashout Washout time in days for the outcome
#' @param censorOnCohortExit If TRUE, an individual's follow up will be
#' censored at their cohort exit
#' @param censorOnDate if not NULL, an individual's follow up will be censored
#' at the given date
#' @param followUpDays Number of days to follow up individuals (lower bound 1,
#' upper bound Inf)
#' @param strata strata
#' @param eventGap Days between time points for which to report survival
#' events, which are grouped into the specified intervals.
#' @param estimateGap Days between time points for which to report survival
#' estimates. First day will be day zero with risk estimates provided
#' for times up to the end of follow-up, with a gap in days equivalent
#' to eventGap.
#' @param restrictedMeanFollowUp number of days of follow-up to take into account
#' when calculating restricted mean for all cohorts
#' @param minimumSurvivalDays Minimum number of days required for the main cohort
#' to have survived
#'
#' @return tibble with survival information for desired cohort, including:
#' time, people at risk, survival probability, cumulative incidence,
#' 95 CIs, strata and outcome. A tibble with the number of events is
#' outputted as an attribute of the output
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(
#'   cdm = cdm,
#'   targetCohortTable = "mgus_diagnosis",
#'   targetCohortId = 1,
#'   outcomeCohortTable = "death_cohort",
#'   outcomeCohortId = 1,
#'   eventGap = 7
#' )
#' }
#'
estimateSingleEventSurvival <- function(cdm,
                                        targetCohortTable,
                                        outcomeCohortTable,
                                        targetCohortId = NULL,
                                        outcomeCohortId = NULL,
                                        outcomeDateVariable = "cohort_start_date",
                                        outcomeWashout = Inf,
                                        censorOnCohortExit = FALSE,
                                        censorOnDate = NULL,
                                        followUpDays = Inf,
                                        strata = NULL,
                                        eventGap = 30,
                                        estimateGap = 1,
                                        restrictedMeanFollowUp = NULL,
                                        minimumSurvivalDays = 1) {
  # Get ids of interest
    if (is.null(targetCohortId)) {
    targetCohortId <- getCohortId(targetCohortTable, cdm)
  }
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- getCohortId(outcomeCohortTable, cdm)
  }

  emptyOutcomes <- omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
    dplyr::left_join(
      cdm[[outcomeCohortTable]] |>
        dplyr::group_by(.data$cohort_definition_id) |>
        dplyr::tally(name = "number_records"),
      by = "cohort_definition_id", copy = TRUE) |>
    dplyr::filter(.data$number_records == 0 | is.na(.data$number_records))
  if(nrow(emptyOutcomes) > 0){
    emptyOutcomenames <- emptyOutcomes |> dplyr::pull("cohort_name")
    cli::cli_warn("Outcome cohort{?s} {emptyOutcomenames} {?is/are} empty")
  }

  emptyTargets <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
    dplyr::left_join(
      cdm[[targetCohortTable]] |>
        dplyr::group_by(.data$cohort_definition_id) |>
        dplyr::tally(name = "number_records"),
      by = "cohort_definition_id", copy = TRUE) |>
    dplyr::filter(.data$number_records == 0 | is.na(.data$number_records))
  if(nrow(emptyTargets) > 0){
    emptyTargetnames <- emptyTargets |> dplyr::pull("cohort_name")
    cli::cli_warn("Target cohort{?s} {emptyTargetnames} {?is/are} empty")
  }

  targetCohortId <- targetCohortId[!(targetCohortId %in% emptyTargets)]

  if(length(outcomeCohortId > 0)) {
    outcomeCohortId <- omopgenerics::validateCohortIdArgument(outcomeCohortId, cdm[[outcomeCohortTable]])
  }
  if(length(targetCohortId > 0)) {
    targetCohortId <- omopgenerics::validateCohortIdArgument(targetCohortId, cdm[[targetCohortTable]])
  }

  # Apply the survival estimation over all combinations of target and outcome cohorts
  results <- expand.grid(target_idx = seq_along(targetCohortId), outcome_idx = seq_along(outcomeCohortId)) |>
    purrr::pmap(function(target_idx, outcome_idx) {

      # Get the target and outcome cohort names
      working_target_id <- targetCohortId[target_idx]
      working_target <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id == working_target_id) |>
        dplyr::pull("cohort_name")
      working_outcome_id <- outcomeCohortId[outcome_idx]
      working_outcome <- omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id == working_outcome_id) |>
        dplyr::pull("cohort_name")
      cli::cli_inform("- Getting survival for target cohort '{working_target}' and outcome cohort '{working_outcome}'")

      # Estimate survival and collect results
      surv <- estimateSurvival(
        cdm = cdm,
        targetCohortTable = targetCohortTable,
        targetCohortId = working_target_id,
        outcomeCohortTable = outcomeCohortTable,
        outcomeCohortId = working_outcome_id,
        outcomeDateVariable = outcomeDateVariable,
        outcomeWashout = outcomeWashout,
        competingOutcomeCohortTable = NULL,
        competingOutcomeCohortId = 1,
        competingOutcomeDateVariable = "cohort_start_date",
        censorOnCohortExit = censorOnCohortExit,
        censorOnDate = censorOnDate,
        followUpDays = followUpDays,
        strata = strata,
        eventGap = eventGap,
        estimateGap = estimateGap,
        restrictedMeanFollowUp = restrictedMeanFollowUp,
        minimumSurvivalDays = minimumSurvivalDays
      )

      # Extract attrition, events, and summary
      attrition <- attr(surv, "cohort_attrition") |>
        dplyr::mutate(target_cohort = working_target, outcome = working_outcome) |>
        dplyr::collect() |>
        dplyr::filter(.data$cohort_definition_id == working_target_id)
      events <- attr(surv, 'events')
      summary <- attr(surv, 'summary')

      # Return the result as a list
      return(list(surv = surv, attrition = attrition, events = events, summary = summary))
    })

  # Extract the results into separate lists
  surv <- lapply(results, `[[`, "surv")
  attrition <- lapply(results, `[[`, "attrition")
  events <- lapply(results, `[[`, "events")
  summary <- lapply(results, `[[`, "summary")

  # Remove empty elements
  surv[lengths(surv) == 0] <- NULL
  events[lengths(events) == 0] <- NULL
  attrition[lengths(attrition) == 0] <- NULL
  summary[lengths(summary) == 0] <- NULL

  # Bind rows for different components
  estimates <- dplyr::bind_rows(surv)
  events <- dplyr::bind_rows(events)
  attrition <- dplyr::bind_rows(attrition)
  summary <- dplyr::bind_rows(summary)

  if(nrow(attrition) > 0) {
    # Clean attrition data
    attrition <- attrition |>
      dplyr::distinct() |>
      dplyr::mutate(
        cdm_name = attr(cdm, "cdm_name"),
        package_name = "CohortSurvival",
        package_version = as.character(utils::packageVersion("CohortSurvival")),
        result_type = "survival_attrition",
        group_name = "target_cohort",
        variable_level = .data$outcome,
        analysis_type = "single_event",
        estimate_name = "count"
      ) |>
      tidyr::pivot_longer(
        cols = c(
          "number_records", "number_subjects", "excluded_records",
          "excluded_subjects"
        ),
        names_to = "variable_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "estimate_value" = as.character(.data$estimate_value),
        "estimate_type" = "integer",
        competing_outcome = "none"
      ) |>
      omopgenerics::uniteStrata("reason") |>
      omopgenerics::uniteAdditional("reason_id")

    if (attrition |> dplyr::group_by("target_cohort") |> dplyr::tally() |> dplyr::pull("n") ==
        attrition |> dplyr::group_by("target_cohort", "cohort_definition_id") |> dplyr::tally() |> dplyr::pull("n")) {
      attrition <- attrition |>
        dplyr::mutate(target_cohort = paste0(.data$target_cohort, "_", .data$cohort_definition_id),
                      group_level = .data$target_cohort)
    }

    attrition <- attrition |>
      dplyr::select(-c("cohort_definition_id"))
  }

  # Check if estimates are not empty
  if(nrow(estimates) > 0) {
    settings <- estimates |>
      dplyr::select("result_type",
                    "package_name",
                    "package_version",
                    "analysis_type",
                    "outcome",
                    "competing_outcome") |>
      dplyr::distinct()

    settings <- settings |>
      dplyr::mutate(eventgap = NA) |>
      dplyr::union_all(
        events |>
          dplyr::select("result_type",
                        "package_name",
                        "package_version",
                        "analysis_type",
                        "outcome",
                        "competing_outcome",
                        "eventgap") |>
          dplyr::distinct()
      )

    settings <- settings |>
      dplyr::union_all(
        summary |>
          dplyr::select("result_type",
                        "package_name",
                        "package_version",
                        "analysis_type",
                        "outcome",
                        "competing_outcome") |>
          dplyr::mutate(eventgap = NA) |>
          dplyr::distinct()
      )

    settings <- settings |>
      dplyr::union_all(
        attrition |>
          dplyr::select("result_type",
                        "analysis_type",
                        "package_name",
                        "package_version",
                        "outcome") |>
          dplyr::mutate(eventgap = NA,
                        competing_outcome = "none") |>
          dplyr::distinct()
      )

    settings <- settings |>
      dplyr::mutate(result_id = c(1:nrow(settings))) |>
      dplyr::relocate("result_id", .before = "result_type")

    estimates <- estimates |>
      dplyr::mutate(additional_name = "time",
                    time = as.character(.data$time)) |>
      dplyr::rename("additional_level" = "time")

    events <- events |>
      dplyr::mutate(additional_name = "time",
                    time = as.character(.data$time)) |>
      dplyr::rename("additional_level" = "time")

    complete_results <- estimates |>
      dplyr::bind_rows(events) |>
      dplyr::bind_rows(summary) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::bind_rows(attrition) |>
      dplyr::left_join(settings,
                       by = c("analysis_type", "package_name", "package_version", "result_type", "outcome", "competing_outcome", "eventgap")) |>
      dplyr::mutate(
        additional_name = dplyr::if_else(is.na(.data$additional_name), "overall", .data$additional_name),
        additional_level = dplyr::if_else(is.na(.data$additional_level), "overall", .data$additional_level)
      )

    complete_results <- complete_results |>
      dplyr::select(omopgenerics::resultColumns())

    settings <- settings |>
      dplyr::mutate(
        outcome_date_variable = .env$outcomeDateVariable,
        outcome_washout = .env$outcomeWashout,
        censor_on_cohort_exit = .env$censorOnCohortExit,
        censor_on_date = .env$censorOnDate,
        follow_up_days = .env$followUpDays,
        restricted_mean_follow_up = .env$restrictedMeanFollowUp,
        minimum_survival_days = .env$minimumSurvivalDays
      )

    surv_estimates <- omopgenerics::newSummarisedResult(complete_results,
                                                        settings = settings)

    attr(surv_estimates, "cohort_attrition") <- NULL
    attr(surv_estimates, "summary") <- NULL
    attr(surv_estimates, "events") <- NULL

    # Suppress results with omopgenerics
    surv_estimates <- surv_estimates |>
      dplyr::mutate(estimate_name = dplyr::if_else(
        .data$estimate_name %in% c("n_risk", "n_events", "n_censor", "number_records"),
        paste0(.data$estimate_name,"_count"), .data$estimate_name
      ))
  } else if (nrow(estimates) == 0 & nrow(attrition) > 0) {
    settings <- attrition |>
      dplyr::select("result_type",
                    "package_name",
                    "package_version",
                    "analysis_type",
                    "outcome",
                    "competing_outcome") |>
      dplyr::distinct() |>
      dplyr::mutate(eventgap = NA,
                    competing_outcome = "none")
    settings <- settings |>
      dplyr::mutate(result_id = c(1:nrow(settings))) |>
      dplyr::relocate("result_id", .before = "result_type")

    complete_results <- attrition |>
      dplyr::mutate(eventgap = NA) |>
      dplyr::left_join(settings,
                       by = c("analysis_type", "package_name", "package_version", "result_type", "outcome", "competing_outcome", "eventgap")) |>
      dplyr::mutate(
        additional_name = dplyr::if_else(is.na(.data$additional_name), "overall", .data$additional_name),
        additional_level = dplyr::if_else(is.na(.data$additional_level), "overall", .data$additional_level)
      )

    complete_results <- complete_results |>
      dplyr::select(omopgenerics::resultColumns())

    settings <- settings |>
      dplyr::mutate(
        outcome_date_variable = .env$outcomeDateVariable,
        outcome_washout = .env$outcomeWashout,
        censor_on_cohort_exit = .env$censorOnCohortExit,
        censor_on_date = .env$censorOnDate,
        follow_up_days = .env$followUpDays,
        restricted_mean_follow_up = .env$restrictedMeanFollowUp,
        minimum_survival_days = .env$minimumSurvivalDays
      )

    surv_estimates <- omopgenerics::newSummarisedResult(complete_results,
                                                        settings = settings)
  } else {
    complete_results <- omopgenerics::emptySummarisedResult()
    surv_estimates <- omopgenerics::newSummarisedResult(complete_results)
  }

  return(surv_estimates)
}

#' Estimate survival for a given event and competing risk of interest using
#' cohorts in the OMOP Common Data Model
#'
#' @param cdm CDM reference
#' @param targetCohortTable The target cohort table of interest.
#' @param outcomeCohortTable The outcome cohort table of interest.
#' @param competingOutcomeCohortTable The competing outcome cohort table of interest.
#' @param targetCohortId Target cohorts to include. It can either be a
#' cohort_definition_id value or a cohort_name. Multiple ids are allowed.
#' @param outcomeCohortId Outcome cohorts to include. It can either be a
#' cohort_definition_id value or a cohort_name. Multiple ids are allowed.
#' @param outcomeDateVariable  Variable containing date of outcome event
#' @param outcomeWashout Washout time in days for the outcome
#' @param competingOutcomeCohortId Competing outcome cohorts to include. It can either be a
#' cohort_definition_id value or a cohort_name. Multiple ids are allowed.
#' @param competingOutcomeDateVariable Variable containing date of competing outcome event
#' @param competingOutcomeWashout Washout time in days for the competing outcome
#' @param censorOnCohortExit If TRUE, an individual's follow up will be
#' censored at their cohort exit
#' @param censorOnDate if not NULL, an individual's follow up will be censored
#' at the given date
#' @param followUpDays Number of days to follow up individuals (lower bound 1,
#' upper bound Inf)
#' @param strata strata
#' @param eventGap Days between time points for which to report survival
#' events, which are grouped into the specified intervals.
#' @param estimateGap Days between time points for which to report survival
#' estimates. First day will be day zero with risk estimates provided
#' for times up to the end of follow-up, with a gap in days equivalent
#' to eventGap.
#' @param restrictedMeanFollowUp number of days of follow-up to take into account
#' when calculating restricted mean for all cohorts
#' @param minimumSurvivalDays Minimum number of days required for the main cohort
#' to have survived
#'
#' @return tibble with survival information for desired cohort, including:
#' time, people at risk, survival probability, cumulative incidence,
#' 95 CIs, strata and outcome. A tibble with the number of events is
#' outputted as an attribute of the output
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateCompetingRiskSurvival(
#'   cdm = cdm,
#'   targetCohortTable = "mgus_diagnosis",
#'   targetCohortId = 1,
#'   outcomeCohortTable = "progression",
#'   outcomeCohortId = 1,
#'   competingOutcomeCohortTable = "death_cohort",
#'   competingOutcomeCohortId = 1,
#'   eventGap = 7
#' )
#' }
#'
estimateCompetingRiskSurvival <- function(cdm,
                                          targetCohortTable,
                                          outcomeCohortTable,
                                          competingOutcomeCohortTable,
                                          targetCohortId = NULL,
                                          outcomeCohortId = NULL,
                                          outcomeDateVariable = "cohort_start_date",
                                          outcomeWashout = Inf,
                                          competingOutcomeCohortId = NULL,
                                          competingOutcomeDateVariable = "cohort_start_date",
                                          competingOutcomeWashout = Inf,
                                          censorOnCohortExit = FALSE,
                                          censorOnDate = NULL,
                                          followUpDays = Inf,
                                          strata = NULL,
                                          eventGap = 30,
                                          estimateGap = 1,
                                          restrictedMeanFollowUp = NULL,
                                          minimumSurvivalDays = 1) {
  # Get ids of interest
  if (is.null(targetCohortId)) {
    targetCohortId <- getCohortId(targetCohortTable, cdm)
  }
  if (is.null(outcomeCohortId)) {
    outcomeCohortId <- getCohortId(outcomeCohortTable, cdm)
  }
  if (is.null(competingOutcomeCohortId)) {
    competingOutcomeCohortId <- getCohortId(competingOutcomeCohortTable, cdm)
  }

  # Handle empty cohorts
  emptyOutcomes <- omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
    dplyr::left_join(
      cdm[[outcomeCohortTable]] |>
        dplyr::group_by(.data$cohort_definition_id) |>
        dplyr::tally(name = "number_records"),
      by = "cohort_definition_id", copy = TRUE) |>
    dplyr::filter(.data$number_records == 0 | is.na(.data$number_records))
  if(nrow(emptyOutcomes) > 0){
    emptyOutcomenames <- emptyOutcomes |> dplyr::pull("cohort_name")
    cli::cli_warn("Outcome cohort{?s} {emptyOutcomenames} {?is/are} empty")
  }

  emptyCompetingOutcomes <- omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) |>
    dplyr::left_join(
      cdm[[competingOutcomeCohortTable]] |>
        dplyr::group_by(.data$cohort_definition_id) |>
        dplyr::tally(name = "number_records"),
      by = "cohort_definition_id", copy = TRUE) |>
    dplyr::filter(.data$number_records == 0 | is.na(.data$number_records))
  if(nrow(emptyCompetingOutcomes) > 0){
    emptyCompetingOutcomenames <- emptyCompetingOutcomes |> dplyr::pull("cohort_name")
    cli::cli_warn("Competing outcome cohort{?s} {emptyCompetingOutcomenames} {?is/are} empty")
  }

  emptyTargets <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
    dplyr::left_join(
      cdm[[targetCohortTable]] |>
        dplyr::group_by(.data$cohort_definition_id) |>
        dplyr::tally(name = "number_records"),
      by = "cohort_definition_id", copy = TRUE) |>
    dplyr::filter(.data$number_records == 0 | is.na(.data$number_records))
  if(nrow(emptyTargets) > 0){
    emptyTargetnames <- emptyTargets |> dplyr::pull("cohort_name")
    cli::cli_warn("Target cohort{?s} {emptyTargetnames} {?is/are} empty")
  }

  targetCohortId <- targetCohortId[!(targetCohortId %in% emptyTargets)]

  if(length(outcomeCohortId > 0)) {
    outcomeCohortId <- omopgenerics::validateCohortIdArgument(outcomeCohortId, cdm[[outcomeCohortTable]])
  }
  if(length(targetCohortId > 0)) {
    targetCohortId <- omopgenerics::validateCohortIdArgument(targetCohortId, cdm[[targetCohortTable]])
  }
  if(length(competingOutcomeCohortId > 0)) {
    competingOutcomeCohortId <- omopgenerics::validateCohortIdArgument(competingOutcomeCohortId, cdm[[competingOutcomeCohortTable]])
  }

  # Apply survival to all combinations of ids
  results <- expand.grid(target_idx = seq_along(targetCohortId), outcome_idx = seq_along(outcomeCohortId),
                         competing_outcome_idx = seq_along(competingOutcomeCohortId)) |>
    purrr::pmap(function(target_idx, outcome_idx, competing_outcome_idx) {

      # Get the target and outcome cohorts names
      working_target_id <- targetCohortId[target_idx]
      working_target <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id == working_target_id) |>
        dplyr::pull("cohort_name")
      working_outcome_id <- outcomeCohortId[outcome_idx]
      working_outcome <- omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id == working_outcome_id) |>
        dplyr::pull("cohort_name")
      working_competing_outcome_id <- competingOutcomeCohortId[competing_outcome_idx]
      working_competing_outcome <- omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id == working_competing_outcome_id) |>
        dplyr::pull("cohort_name")
      cli::cli_inform("- Getting survival for target cohort '{working_target}', outcome cohort '{working_outcome}' and
                      competing outcome cohort '{working_competing_outcome}'")

      # Estimate survival and collect results
      surv <- estimateSurvival(
        cdm = cdm,
        targetCohortTable = targetCohortTable,
        targetCohortId = working_target_id,
        outcomeCohortTable = outcomeCohortTable,
        outcomeCohortId = working_outcome_id,
        outcomeDateVariable = outcomeDateVariable,
        outcomeWashout = outcomeWashout,
        competingOutcomeCohortTable = competingOutcomeCohortTable,
        competingOutcomeCohortId = working_competing_outcome_id,
        competingOutcomeDateVariable = competingOutcomeDateVariable,
        competingOutcomeWashout = competingOutcomeWashout,
        censorOnCohortExit = censorOnCohortExit,
        censorOnDate = censorOnDate,
        followUpDays = followUpDays,
        strata = strata,
        eventGap = eventGap,
        estimateGap = estimateGap,
        restrictedMeanFollowUp = restrictedMeanFollowUp,
        minimumSurvivalDays = minimumSurvivalDays
      )

      # Extract attrition, events, and summary
      if(!is.null(attr(surv, "cohort_attrition"))) {
        attrition <- attr(surv, "cohort_attrition") |>
          dplyr::mutate(target_cohort = working_target, outcome = working_outcome,
                        competing_outcome = working_competing_outcome) |>
          dplyr::collect() |>
          dplyr::filter(.data$cohort_definition_id == working_target_id)
      } else {
        attrition <- attr(surv, "cohort_attrition")
      }
      events <- attr(surv, 'events')
      summary <- attr(surv, 'summary')

      # Return the result as a list
      return(list(surv = surv, attrition = attrition, events = events, summary = summary))
    })

  # Extract the results into separate lists
  surv <- lapply(results, `[[`, "surv")
  attrition <- lapply(results, `[[`, "attrition")
  events <- lapply(results, `[[`, "events")
  summary <- lapply(results, `[[`, "summary")

  # Remove empty elements
  surv[lengths(surv) == 0] <- NULL
  events[lengths(events) == 0] <- NULL
  attrition[lengths(attrition) == 0] <- NULL
  summary[lengths(summary) == 0] <- NULL

  # Bind rows for different components
  estimates <- dplyr::bind_rows(surv)
  events <- dplyr::bind_rows(events)
  attrition <- dplyr::bind_rows(attrition)
  summary <- dplyr::bind_rows(summary)

  if(nrow(attrition) > 0) {
    # Clean attrition data
    attrition <- attrition |>
      dplyr::distinct() |>
      dplyr::mutate(
        cdm_name = attr(cdm, "cdm_name"),
        package_name = "CohortSurvival",
        package_version = as.character(utils::packageVersion("CohortSurvival")),
        result_type = "survival_attrition",
        group_name = "target_cohort",
        variable_level = .data$outcome,
        analysis_type = "competing_risk",
        estimate_name = "count"
      ) |>
      tidyr::pivot_longer(
        cols = c(
          "number_records", "number_subjects", "excluded_records",
          "excluded_subjects"
        ),
        names_to = "variable_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        "estimate_value" = as.character(.data$estimate_value),
        "estimate_type" = "integer",
        competing_outcome = "none"
      ) |>
      omopgenerics::uniteStrata("reason") |>
      omopgenerics::uniteAdditional("reason_id")

    if (attrition |> dplyr::group_by("target_cohort") |> dplyr::tally() |> dplyr::pull("n") ==
        attrition |> dplyr::group_by("target_cohort", "cohort_definition_id") |> dplyr::tally() |> dplyr::pull("n")) {
      attrition <- attrition |>
        dplyr::mutate(target_cohort = paste0(.data$target_cohort, "_", .data$cohort_definition_id),
                      group_level = .data$target_cohort)
    }

    attrition <- attrition |>
      dplyr::select(-c("cohort_definition_id"))
  }

  # Check if estimates are not empty
  if(nrow(estimates) > 0) {
    settings <- estimates |>
      dplyr::select("result_type",
                    "package_name",
                    "package_version",
                    "analysis_type",
                    "outcome",
                    "competing_outcome") |>
      dplyr::distinct()

    settings <- settings |>
      dplyr::mutate(eventgap = NA) |>
      dplyr::union_all(
        events |>
          dplyr::select("result_type",
                        "package_name",
                        "package_version",
                        "analysis_type",
                        "outcome",
                        "competing_outcome",
                        "eventgap") |>
          dplyr::distinct()
      )

    settings <- settings |>
      dplyr::union_all(
        summary |>
          dplyr::select("result_type",
                        "package_name",
                        "package_version",
                        "analysis_type",
                        "outcome",
                        "competing_outcome") |>
          dplyr::mutate(eventgap = NA) |>
          dplyr::distinct()
      )

    settings <- settings |>
      dplyr::union_all(
        attrition |>
          dplyr::select("result_type",
                        "analysis_type",
                        "package_name",
                        "package_version",
                        "outcome") |>
          dplyr::mutate(eventgap = NA,
                        competing_outcome = "none") |>
          dplyr::distinct()
      )

    settings <- settings |>
      dplyr::mutate(result_id = c(1:nrow(settings))) |>
      dplyr::relocate("result_id", .before = "result_type")

    estimates <- estimates |>
      dplyr::mutate(additional_name = "time",
                    time = as.character(.data$time)) |>
      dplyr::rename("additional_level" = "time")

    events <- events |>
      dplyr::mutate(additional_name = "time",
                    time = as.character(.data$time)) |>
      dplyr::rename("additional_level" = "time")

    complete_results <- estimates |>
      dplyr::bind_rows(events) |>
      dplyr::bind_rows(summary) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)) |>
      dplyr::bind_rows(attrition) |>
      dplyr::left_join(settings,
                       by = c("analysis_type", "package_name", "package_version", "result_type", "outcome", "competing_outcome", "eventgap")) |>
      dplyr::mutate(
        additional_name = dplyr::if_else(is.na(.data$additional_name), "overall", .data$additional_name),
        additional_level = dplyr::if_else(is.na(.data$additional_level), "overall", .data$additional_level)
      )

    complete_results <- complete_results |>
      dplyr::select(omopgenerics::resultColumns())

    settings <- settings |>
      dplyr::mutate(
        outcome_date_variable = .env$outcomeDateVariable,
        outcome_washout = .env$outcomeWashout,
        censor_on_cohort_exit = .env$censorOnCohortExit,
        censor_on_date = .env$censorOnDate,
        follow_up_days = .env$followUpDays,
        restricted_mean_follow_up = .env$restrictedMeanFollowUp,
        minimum_survival_days = .env$minimumSurvivalDays
      )

    surv_estimates <- omopgenerics::newSummarisedResult(complete_results,
                                                        settings = settings)

    attr(surv_estimates, "cohort_attrition") <- NULL
    attr(surv_estimates, "summary") <- NULL
    attr(surv_estimates, "events") <- NULL

    # Suppress results with omopgenerics
    surv_estimates <- surv_estimates |>
      dplyr::mutate(estimate_name = dplyr::if_else(
        .data$estimate_name %in% c("n_risk", "n_events", "n_censor", "number_records"),
        paste0(.data$estimate_name,"_count"), .data$estimate_name
      ))
  } else if (nrow(estimates) == 0 & nrow(attrition) > 0) {
    settings <- attrition |>
      dplyr::select("result_type",
                    "package_name",
                    "package_version",
                    "analysis_type",
                    "outcome",
                    "competing_outcome") |>
      dplyr::distinct() |>
      dplyr::mutate(eventgap = NA,
                    competing_outcome = "none")
    settings <- settings |>
      dplyr::mutate(result_id = c(1:nrow(settings))) |>
      dplyr::relocate("result_id", .before = "result_type")

    complete_results <- attrition |>
      dplyr::mutate(eventgap = NA) |>
      dplyr::left_join(settings,
                       by = c("analysis_type", "package_name", "package_version", "result_type", "outcome", "competing_outcome", "eventgap")) |>
      dplyr::mutate(
        additional_name = dplyr::if_else(is.na(.data$additional_name), "overall", .data$additional_name),
        additional_level = dplyr::if_else(is.na(.data$additional_level), "overall", .data$additional_level)
      )

    complete_results <- complete_results |>
      dplyr::select(omopgenerics::resultColumns())

    settings <- settings |>
      dplyr::mutate(
        outcome_date_variable = .env$outcomeDateVariable,
        outcome_washout = .env$outcomeWashout,
        censor_on_cohort_exit = .env$censorOnCohortExit,
        censor_on_date = .env$censorOnDate,
        follow_up_days = .env$followUpDays,
        restricted_mean_follow_up = .env$restrictedMeanFollowUp,
        minimum_survival_days = .env$minimumSurvivalDays
      )

    surv_estimates <- omopgenerics::newSummarisedResult(complete_results,
                                                        settings = settings)
  } else {
    complete_results <- omopgenerics::emptySummarisedResult()
    surv_estimates <- omopgenerics::newSummarisedResult(complete_results)
  }

  return(surv_estimates)
}


estimateSurvival <- function(cdm,
                             targetCohortTable,
                             targetCohortId = NULL,
                             outcomeCohortTable,
                             outcomeCohortId = NULL,
                             outcomeDateVariable = "cohort_start_date",
                             outcomeWashout = Inf,
                             competingOutcomeCohortTable = NULL,
                             competingOutcomeCohortId = 1,
                             competingOutcomeDateVariable = "cohort_start_date",
                             competingOutcomeWashout = Inf,
                             censorOnCohortExit = FALSE,
                             censorOnDate = NULL,
                             followUpDays = Inf,
                             strata = NULL,
                             eventGap = 30,
                             estimateGap = 1,
                             restrictedMeanFollowUp = NULL,
                             minimumSurvivalDays = 1) {

  # check inputs
  omopgenerics::assertCharacter(targetCohortTable, length = 1)
  omopgenerics::assertCharacter(outcomeCohortTable, length = 1)
  omopgenerics::assertCharacter(competingOutcomeCohortTable, length = 1, null = TRUE)
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::validateCohortArgument(cdm[[targetCohortTable]])
  omopgenerics::validateCohortArgument(cdm[[outcomeCohortTable]])
  omopgenerics::assertNumeric(targetCohortId, integerish = TRUE, length = 1, min = 1)
  omopgenerics::assertList(strata, null = TRUE)
  omopgenerics::assertChoice(unlist(strata), choices = colnames(cdm[[targetCohortTable]]), null = TRUE)
  omopgenerics::assertCharacter(outcomeDateVariable, length = 1)
  omopgenerics::assertCharacter(competingOutcomeDateVariable, length = 1)
  omopgenerics::assertLogical(censorOnCohortExit, length = 1)
  omopgenerics::assertDate(censorOnDate, null = TRUE)
  omopgenerics::assertNumeric(followUpDays, length = 1, min = 1, integerish = TRUE)
  omopgenerics::assertNumeric(outcomeWashout, length = 1, min = 0, integerish = TRUE)
  omopgenerics::assertNumeric(competingOutcomeWashout, length = 1, min = 0, integerish = TRUE)
  omopgenerics::assertNumeric(eventGap, integerish = TRUE, min = 1)
  omopgenerics::assertNumeric(estimateGap, integerish = TRUE, min = 1)
  omopgenerics::assertNumeric(restrictedMeanFollowUp, integerish = TRUE, min = 1, length = 1, null = TRUE)
  omopgenerics::assertNumeric(minimumSurvivalDays, integerish = TRUE, min = 0, length = 1)

# extract and prepare exposure data
  workingExposureTable <- cdm[[targetCohortTable]] |>
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId) |>
    addCohortSurvival(cdm, outcomeCohortTable, outcomeCohortId, outcomeDateVariable,
      outcomeWashout, censorOnCohortExit, censorOnDate, followUpDays) |>
    dplyr::rename( "outcome_time" = "time", "outcome_status" = "status") |>
    dplyr::compute(temporary = FALSE,
                   logPrefix = "CohortSurvival_estimateSurvival_exposure")

  # handle competing risks
  if (!is.null(competingOutcomeCohortTable)) {
    workingExposureTable <- workingExposureTable |>
      addCohortSurvival(cdm, competingOutcomeCohortTable, competingOutcomeCohortId,
        competingOutcomeDateVariable, competingOutcomeWashout, censorOnCohortExit,
        censorOnDate, followUpDays) |>
      dplyr::rename("competing_risk_time" = "time", "competing_risk_status" = "status") |>
      dplyr::compute(temporary = FALSE,
                     logPrefix = "CohortSurvival_estimateSurvival_add_competing_risk")
  }

  # collect
  workingExposureTable <- workingExposureTable |>
    dplyr::filter(!is.na(.data$outcome_time) &
                    !is.na(.data$outcome_status)) |>
    dplyr::compute(temporary = FALSE,
                   logPrefix = "CohortSurvival_estimateSurvival_remove_na") |>
    omopgenerics::recordCohortAttrition(reason = "No outcome event in washout period")

  workingExposureTable <- workingExposureTable |>
    dplyr::filter(.data$outcome_time >= .env$minimumSurvivalDays) |>
  omopgenerics::recordCohortAttrition(reason = paste0("Survival days for outcome less than ", minimumSurvivalDays))

  if (!is.null(competingOutcomeCohortTable)) {
    workingExposureTable <- workingExposureTable |>
      dplyr::filter(.data$competing_risk_time >= .env$minimumSurvivalDays) |>
      omopgenerics::recordCohortAttrition(reason = paste0("Survival days for competing outcome less than ", minimumSurvivalDays))
  }

  survData <- workingExposureTable |>
    dplyr::collect()

  if (!is.null(competingOutcomeCohortTable)) {
    survData <- addCompetingRiskVars(data = survData, time1 = "outcome_time",
      status1 = "outcome_status", time2 = "competing_risk_time",
      status2 = "competing_risk_status", nameOutTime = "outcome_or_competing_time",
      nameOutStatus = "outcome_or_competing_status")
  }

  # time points to extract survival estimates
  if(survData |> dplyr::tally() |> dplyr::pull() != 0) {
    timepoints <- seq(0, if (followUpDays == "Inf") max(survData$outcome_time) else followUpDays, by = estimateGap)
  } else {
    working_target <- omopgenerics::settings(cdm[[targetCohortTable]]) |>
      dplyr::filter(.data$cohort_definition_id == targetCohortId) |>
      dplyr::pull("cohort_name")
    working_outcome <- omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
      dplyr::filter(.data$cohort_definition_id == outcomeCohortId) |>
      dplyr::pull("cohort_name")
    if(!is.null(competingOutcomeCohortTable)) {
    working_competing_outcome <- omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) |>
      dplyr::filter(.data$cohort_definition_id == competingOutcomeCohortId) |>
      dplyr::pull("cohort_name")
      cli::cli_warn("Survival estimation is not possible as there are no individuals
                  available for target {working_target}, outcome
                  {working_outcome} and competing outcome {working_competing_outcome}.
                    Only attrition returned.")
    } else {
      cli::cli_warn("Survival estimation is not possible as there are no individuals
                  available for target {working_target} and outcome
                  {working_outcome}. Only attrition returned.")
    }
    surv <- empty_estimates()
    attr(surv, "cohort_attrition") <- attr(workingExposureTable, "cohort_attrition")
    return(surv)
    }

  # fit survival, with strata
  if (is.null(competingOutcomeCohortTable)) {
    surv <- singleEventSurvival(survData, timepoints, strata, eventGap, restrictedMeanFollowUp)
  } else {
    surv <- competingRiskSurvival(survData, timepoints, strata, eventGap, restrictedMeanFollowUp)
  }

  # process and summarise results
  if (nrow(surv) > 0) {
    survivalEstimates <- addCohortDetails(surv, cdm, targetCohortId, targetCohortTable,
      outcomeCohortId, outcomeCohortTable, competingOutcomeCohortId, competingOutcomeCohortTable,
      "survival") |>
      tidyr::pivot_longer(cols = "outcome", names_to = "variable_name",
        values_to = "variable_level") |>
      dplyr::mutate(result_type = "survival_estimates") |>
      dplyr::select(!c("n_risk","variable_type", "n_censor")) |>
      tidyr::pivot_longer(
        cols = c("estimate", "estimate_95CI_lower", "estimate_95CI_upper"),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(
        outcome = omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
          dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) |>
          dplyr::pull("cohort_name")
      )

    if(!is.null(competingOutcomeCohortTable)) {
      survivalEstimates <- survivalEstimates |>
        dplyr::mutate(competing_outcome = omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) |>
                                         dplyr::filter(.data$cohort_definition_id == .env$competingOutcomeCohortId) |>
                                         dplyr::pull("cohort_name"))
    } else {
      survivalEstimates <- survivalEstimates |>
        dplyr::mutate(competing_outcome = "none")
    }

    survivalEstimates <- dplyr::distinct(survivalEstimates)

    # add attributes
    events <- attr(surv, "events")

    attr(survivalEstimates, "events") <- addCohortDetails(events, cdm,
      targetCohortId, targetCohortTable, outcomeCohortId,
      outcomeCohortTable, competingOutcomeCohortId, competingOutcomeCohortTable,
      "survival_events") |>
      dplyr::select(!"variable_type") |>
      tidyr::pivot_longer(cols = c("n_risk", "n_events", "n_censor"),
                          names_to = "estimate_name",
                          values_to = "estimate_value") |>
      dplyr::mutate(
        estimate_type = "numeric",
        variable_name = "outcome"
      ) |>
      dplyr::rename(variable_level = "outcome") |>
      dplyr::mutate(outcome = omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
                      dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) |>
                      dplyr::pull("cohort_name")) |>
      dplyr::rename("eventgap" = "eventGap")

    if(!is.null(competingOutcomeCohortTable)) {
      attr(survivalEstimates, "events") <- attr(survivalEstimates, "events") |>
        dplyr::mutate(competing_outcome = omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) |>
                        dplyr::filter(.data$cohort_definition_id == .env$competingOutcomeCohortId) |>
                        dplyr::pull("cohort_name"))
    } else {
      attr(survivalEstimates, "events") <- attr(survivalEstimates, "events") |>
        dplyr::mutate(competing_outcome = "none")
    }

    attr(survivalEstimates, "cohort_attrition") <- attr(workingExposureTable, "cohort_attrition")

    if (is.null(competingOutcomeCohortTable)) {
      summary <- addCohortDetails(
        x = attr(surv, "summary"), cdm, targetCohortId, targetCohortTable,
        outcomeCohortId, outcomeCohortTable, resultType = "survival_summary"
      ) |>
        dplyr::mutate(analysis_type = "single_event")
    } else {
      attr(surv, "summary") <- attr(surv, "summary") |>
        dplyr::filter(.data$outcome != "none")

      summary <- addCohortDetails(
        x = attr(surv, "summary"), cdm, targetCohortId, targetCohortTable,
        outcomeCohortId, outcomeCohortTable, competingOutcomeCohortId,
        competingOutcomeCohortTable, "survival_summary"
      ) |>
        dplyr::mutate(analysis_type = "competing_risk")
    }

    summary <- summary |>
      dplyr::mutate(
        restricted_mean_survival_95CI_upper = .data$restricted_mean_survival + stats::qnorm(0.975) * .data$restricted_mean_survival_se,
        restricted_mean_survival_95CI_lower = .data$restricted_mean_survival - stats::qnorm(0.975) * .data$restricted_mean_survival_se
      ) |>
      dplyr::select(-"restricted_mean_survival_se")
    summary <- summary |>
      dplyr::filter(.data$outcome != "none") |>
      dplyr::mutate(
        variable_name = "outcome",
        variable_level = .data$outcome,
        estimate_type = "numeric"
      ) |>
      dplyr::select(-c("variable_type", "outcome")) |>
      tidyr::pivot_longer(
        cols = -c("cdm_name", "package_name", "package_version", "result_type", "group_name", "group_level",
                  "strata_name", "strata_level", "variable_name", "variable_level", "estimate_type", "analysis_type"),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) |>
      dplyr::mutate(estimate_value = round(.data$estimate_value),
                    outcome = omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
                      dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) |>
                      dplyr::pull("cohort_name"))

    if(!is.null(competingOutcomeCohortTable)) {
      summary <- summary |>
        dplyr::mutate(competing_outcome = omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) |>
                        dplyr::filter(.data$cohort_definition_id == .env$competingOutcomeCohortId) |>
                        dplyr::pull("cohort_name"))
    } else {
      summary <- summary |>
        dplyr::mutate(competing_outcome = "none")
    }

    # median_survival and its two confidence intervals must be all num or all NA
    summary <- summary |>
      dplyr::group_by(.data$group_level, .data$variable_level, .data$strata_name, .data$strata_level) |>
      dplyr::mutate(
        has_na = any(is.na(.data$estimate_value) & .data$estimate_name %in% c("median_survival", "median_survival_95CI_higher", "median_survival_95CI_lower")),
        estimate_value = dplyr::if_else(.data$has_na & .data$estimate_name %in% c("median_survival", "median_survival_95CI_higher", "median_survival_95CI_lower"), NA, .data$estimate_value)
      ) |>
      dplyr::select(-"has_na") |>
      dplyr::ungroup()

    # change restrictedMeanSurvival to NA if follow-up for the specific group is shorter than input asked
    if(!is.null(restrictedMeanFollowUp)) {
      summary <- summary |>
        dplyr::left_join(
          survivalEstimates |> dplyr::group_by(.data$variable_level,
                                               .data$strata_name,
                                               .data$strata_level) |>
            dplyr::filter(.data$time == max(.data$time)) |>
            dplyr::select("variable_level",
                          "strata_name",
                          "strata_level",
                          "time") |>
            dplyr::distinct() |>
            dplyr::filter(.data$time < .env$restrictedMeanFollowUp) |>
            dplyr::mutate(to_na = 1),
          by = c("variable_level", "strata_name", "strata_level")) |>
        dplyr::mutate(estimate_value = dplyr::if_else(
          .data$estimate_name %in% c("restricted_mean_survival", "restricted_mean_survival_se") &
            !is.na(.data$to_na),
          NA_real_,
          .data$estimate_value
        )) |>
        dplyr::select(-"to_na")
    }

    attr(survivalEstimates, "summary") <- summary

    # round estimates
    survivalEstimates <- survivalEstimates |>
      dplyr::mutate(estimate_value = round(.data$estimate_value, 4))

  } else {
    survivalEstimates <- surv
  }

  return(survivalEstimates)
}

addCompetingRiskVars <- function(data, time1, status1, time2, status2,
                                 nameOutTime, nameOutStatus) {
  # - add competing risk variables (time and status)
  # 0: no event, 1: event 1, 2: event 2
  data |>
    dplyr::mutate(
      !!nameOutTime := pmin(.data[[time1]], .data[[time2]]),
      !!nameOutStatus := factor(dplyr::if_else(.data[[time2]] <= .data[[time1]], 2 * .data[[status2]], .data[[status1]]))
    )
}

singleEventSurvival <- function(survData, times, variables, eventGap,
                                restrictedMeanFollowUp = NULL) {
  estimates <- list()
  fitSummary <- list()

  var_columns <- unlist(variables) |> unique()

  cli::cli_progress_message("Getting overall estimates")
  fit <- survival::survfit(survival::Surv(outcome_time, outcome_status) ~ 1,
                           data = survData
  )

  # Calculate quantiles
  q0 <- stats::quantile(fit, probs = 0)
  q05 <- stats::quantile(fit, probs = 0.05)
  q25 <- stats::quantile(fit, probs = 0.25)
  q75 <- stats::quantile(fit, probs = 0.75)
  q95 <- stats::quantile(fit, probs = 0.95)
  q100 <- stats::quantile(fit, probs = 1)

  # Create summary with selected and renamed columns
  fitSummary[[1]] <- as.data.frame(t(summary(fit, rmean = restrictedMeanFollowUp)$table)) |>
    dplyr::select(!dplyr::any_of(c("n.max", "n.start"))) |>
    dplyr::rename(
      "number_records" = "records",
      "n_events" = "events",
      "median_survival" = "median",
      "median_survival_95CI_lower" = "0.95LCL",
      "median_survival_95CI_higher" = "0.95UCL",
      "restricted_mean_survival" = "rmean",
      "restricted_mean_survival_se" = "se(rmean)"
    ) |>
    dplyr::mutate(
      q0_survival = .env$q0$quantile,
      q0_survival_95CI_lower = .env$q0$lower,
      q0_survival_95CI_higher = .env$q0$upper,
      q05_survival = .env$q05$quantile,
      q05_survival_95CI_lower = .env$q05$lower,
      q05_survival_95CI_higher = .env$q05$upper,
      q25_survival = .env$q25$quantile,
      q25_survival_95CI_lower = .env$q25$lower,
      q25_survival_95CI_higher = .env$q25$upper,
      q75_survival = .env$q75$quantile,
      q75_survival_95CI_lower = .env$q75$lower,
      q75_survival_95CI_higher = .env$q75$upper,
      q95_survival = .env$q95$quantile,
      q95_survival_95CI_lower = .env$q95$lower,
      q95_survival_95CI_higher = .env$q95$upper,
      q100_survival = .env$q100$quantile,
      q100_survival_95CI_lower = .env$q100$lower,
      q100_survival_95CI_higher = .env$q100$upper,
      analysis_type = "single event",
      strata_name = "overall",
      strata_level = "overall",
      outcome = "outcome"
    )

  summ <- summary(fit, times = times, extend = TRUE)
  estimates[[1]] <- dplyr::tibble(
      outcome = "outcome",
      time = summ$time,
      n_event = summ$n.event,
      n_censor = summ$n.censor,
      n_risk = summ$n.risk,
      estimate_type = "numeric",
      estimate = summ$surv,
      estimate_95CI_lower = summ$lower,
      estimate_95CI_upper = summ$upper,
      analysis_type = "single_event",
      strata_name = "overall",
      strata_level = "overall"
    )

  # Add strata estimates if required
  if (!is.null(variables)) {
    cli::cli_progress_bar(
      total = length(variables),
      format = " -- Getting estimates for {cli::pb_bar} {cli::pb_current} of {cli::pb_total} strata"
    )
    for (i in seq_along(variables)) {
      cli::cli_progress_update()
      # Get formula for the model
      name <- variables[[i]]
      uniqueVals <- survData |>
        dplyr::select(.env$name) |>
        tidyr::unite(col = "vars") |>
        dplyr::distinct() |>
        dplyr::pull()

      if(length(uniqueVals) == 1){
        cli::cli_inform("All values of {name} are equal to {uniqueVals}
                        and stratified results will not be returned for
                        this variable")
      } else {
        expr <- stats::as.formula(paste(c(
          "survival::Surv(outcome_time, outcome_status) ~ 1",
          name
        ), collapse = " + "))
        fit <- survival::survfit(expr, data = survData)
        tidyFit <- broom::tidy(fit)

        format_surv_strata_quantile <- function(x) {
          data.frame(x) |>
            tibble::rownames_to_column(var = "strata")
        }
        q0 <- stats::quantile(fit, probs = 0)
        q0 <- lapply(q0, format_surv_strata_quantile)
        q05 <- stats::quantile(fit, probs = 0.05)
        q05 <- lapply(q05, format_surv_strata_quantile)
        q25 <- stats::quantile(fit, probs = 0.25)
        q25 <- lapply(q25, format_surv_strata_quantile)
        q75 <- stats::quantile(fit, probs = 0.75)
        q75 <- lapply(q75, format_surv_strata_quantile)
        q95 <- stats::quantile(fit, probs = 0.95)
        q95 <- lapply(q95, format_surv_strata_quantile)
        q100 <- stats::quantile(fit, probs = 1)
        q100 <- lapply(q100, format_surv_strata_quantile)

        fitSummary[[i + 1]] <- as.data.frame(summary(fit, rmean = restrictedMeanFollowUp)$table) |>
          dplyr::select(!dplyr::any_of(c("n.max", "n.start"))) |>
          dplyr::rename(
            "number_records" = "records",
            "n_events" = "events",
            "median_survival" = "median",
            "median_survival_95CI_lower" = "0.95LCL",
            "median_survival_95CI_higher" = "0.95UCL",
            "restricted_mean_survival" = "rmean",
            "restricted_mean_survival_se" = "se(rmean)"
          ) |>
          tibble::rownames_to_column(var = "strata")

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] |>
          dplyr::left_join(q0$quantile, by = "strata") |>
          dplyr::rename("q0_survival" = "X0") |>
          dplyr::left_join(q0$lower, by = "strata") |>
          dplyr::rename("q0_survival_95CI_lower" = "X0") |>
          dplyr::left_join(q0$upper, by = "strata") |>
          dplyr::rename("q0_survival_95CI_higher" = "X0") |>
          dplyr::left_join(q05$quantile, by = "strata") |>
          dplyr::rename("q05_survival" = "X5") |>
          dplyr::left_join(q05$lower, by = "strata") |>
          dplyr::rename("q05_survival_95CI_lower" = "X5") |>
          dplyr::left_join(q05$upper, by = "strata") |>
          dplyr::rename("q05_survival_95CI_higher" = "X5") |>
          dplyr::left_join(q25$quantile, by = "strata") |>
          dplyr::rename("q25_survival" = "X25") |>
          dplyr::left_join(q25$lower, by = "strata") |>
          dplyr::rename("q25_survival_95CI_lower" = "X25") |>
          dplyr::left_join(q25$upper, by = "strata") |>
          dplyr::rename("q25_survival_95CI_higher" = "X25") |>
          dplyr::left_join(q75$quantile, by = "strata") |>
          dplyr::rename("q75_survival" = "X75") |>
          dplyr::left_join(q75$lower, by = "strata") |>
          dplyr::rename("q75_survival_95CI_lower" = "X75") |>
          dplyr::left_join(q75$upper, by = "strata") |>
          dplyr::rename("q75_survival_95CI_higher" = "X75") |>
          dplyr::left_join(q95$quantile, by = "strata") |>
          dplyr::rename("q95_survival" = "X95") |>
          dplyr::left_join(q95$lower, by = "strata") |>
          dplyr::rename("q95_survival_95CI_lower" = "X95") |>
          dplyr::left_join(q95$upper, by = "strata") |>
          dplyr::rename("q95_survival_95CI_higher" = "X95") |>
          dplyr::left_join(q100$quantile, by = "strata") |>
          dplyr::rename("q100_survival" = "X100") |>
          dplyr::left_join(q100$lower, by = "strata") |>
          dplyr::rename("q100_survival_95CI_lower" = "X100") |>
          dplyr::left_join(q100$upper, by = "strata") |>
          dplyr::rename("q100_survival_95CI_higher" = "X100")

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] |>
          dplyr::left_join(
            tidyFit |>
              dplyr::group_by(.data$strata) |>
              dplyr::summarise(max_time = max(.data$time, na.rm = TRUE)),
            by = "strata"
          ) |>
          dplyr::mutate(
            analysis_type = "single event",
            outcome = "outcome"
          )

        summ <- summary(fit, times = times, extend = TRUE)
        estimates[[i + 1]] <- dplyr::tibble(
            strata = summ$strata,
            outcome = "outcome",
            time = summ$time,
            n_event = summ$n.event,
            n_censor = summ$n.censor,
            n_risk = summ$n.risk,
            estimate_type = "numeric",
            estimate = summ$surv,
            estimate_95CI_lower = summ$lower,
            estimate_95CI_upper = summ$upper,
            analysis_type = "single_event")

        # Add strata variable columns in a good format
        for (j in seq_along(name)) {
          name_w <- name
          estimates[[i + 1]] <- estimates[[i + 1]] |>
            dplyr::mutate(
              strata_name = paste(name_w, collapse = " &&& "),
              strata_level = rep(gsub(", ", " &&& ", gsub(
                paste(paste0(name_w, "="),
                      collapse = "|"
                ), "",
                summ$strata
              )), 1)
            )

          fitSummary[[i + 1]] <- fitSummary[[i + 1]] |>
            dplyr::mutate(
              strata_name = paste(name_w, collapse = " &&& "),
              strata_level = gsub(", ", " &&& ", gsub(
                paste(paste0(name_w, "="),
                      collapse = "|"
                ), "",
                fitSummary[[i + 1]]$strata
              ))
            )
        }

        # keep estimated probabilities only up to the end of follow up for that group
        maxTimePoints <- fitSummary[[i + 1]] |>
          dplyr::select("strata", "max_time")

        estimates[[i + 1]] <- estimates[[i + 1]] |>
          dplyr::left_join(maxTimePoints,
                           by = "strata"
          ) |>
          dplyr::filter(.data$time <= .data$max_time) |>
          dplyr::select(!c("max_time", "strata"))

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] |>
          dplyr::select(!c("max_time", "strata"))
      }
    }
    cli::cli_progress_done()
  }

  estimates <- dplyr::bind_rows(estimates) |>
    dplyr::mutate(
      strata_level = stringr::str_trim(.data$strata_level)
    )

  # Helper function to calculate events for a given eventGap
  calculate_events <- function(data, gap) {
    data |>
      dplyr::filter(.data$estimate_type == "numeric") |>
      dplyr::group_by(.data$strata_name, .data$strata_level) |>
      dplyr::mutate(
        n_events = cumsum(.data$n_event),
        n_censor = cumsum(.data$n_censor)
      ) |>
      dplyr::filter(.data$time %% gap == 0 | .data$time == max(.data$time)) |>
      dplyr::mutate(
        n_events = c(.data$n_events[1], diff(.data$n_events)),
        n_censor = c(.data$n_censor[1], diff(.data$n_censor)),
        eventGap = gap,
        outcome = "outcome"
      ) |>
      dplyr::ungroup() |>
      dplyr::select("time", "n_risk", "n_events", "n_censor", "eventGap", "outcome", "strata_name", "strata_level")
  }

  # Calculate number of events for all eventGaps
  number_events <- purrr::map_dfr(eventGap, ~calculate_events(estimates, .x))

  estimates <- estimates |>
    dplyr::select(!"n_event")

  attr(estimates, "events") <- number_events
  attr(estimates, "summary") <- dplyr::as_tibble(dplyr::bind_rows(fitSummary))

  return(estimates)
}

competingRiskSurvival <- function(survData, times, variables, eventGap,
                                  restrictedMeanFollowUp = NULL) {
  if (!length(unique(as.character(survData |>
                                  dplyr::pull("outcome_or_competing_status")))) == 3) {
    cli::cli_h1("No results for competing risk analysis")
    cli::cli_text(c(
      "Competing risk variable must have three levels.",
      "Do you have at least 1 individual for:"
    ))
    cli::cli_li("1) censored without event,")
    cli::cli_li("2) censored at outcome event of intest, and")
    cli::cli_li("3) censored at outcome competing event?")

    return(empty_estimates())
  }

  estimates <- list()
  fitSummary <- list()

  var_columns <- unlist(variables) |> unique()

  cli::cli_progress_message("Getting overall estimates")
  fit <- survival::survfit(
    formula = survival::Surv(
      outcome_or_competing_time,
      outcome_or_competing_status
    ) ~ 1,
    data = survData
  )
  summ <- summary(fit, times = times, extend = TRUE, data.frame = TRUE)

  fitSummary[[1]] <- as.data.frame(summary(fit, rmean = restrictedMeanFollowUp)$table) |>
    dplyr::rename(
      "number_records" = "n",
      "n_events" = "nevent",
      "restricted_mean_survival" = "rmean",
      "restricted_mean_survival_se" = "se(rmean)"
    ) |>
    dplyr::mutate(analysis_type = "competing_risk") |>
    dplyr::mutate(
      strata_name = "overall",
      strata_level = "overall"
    ) |>
    tibble::rownames_to_column(var = "outcome") |>
    dplyr::mutate(outcome = dplyr::if_else(.data$outcome == "(s0)", "none",
                                           dplyr::if_else(.data$outcome == "1",
                                                          "outcome", "competing_outcome"
                                           )
    ))

  estimates[[1]] <- summ |>
    dplyr::rename(
      "estimate" = "pstate",
      "estimate_95CI_lower" = "lower",
      "estimate_95CI_upper" = "upper",
      "outcome" = "state",
      "n_risk" = "n.risk",
      "n_event" = "n.event",
      "n_censor" = "n.censor"
    ) |>
    dplyr::mutate(outcome = dplyr::if_else(.data$outcome == 1,
                                           "outcome", dplyr::if_else(.data$outcome == 2,
                                                                     "competing_outcome", "none"))
    ) |>
    dplyr::mutate(analysis_type = "competing_risk",
                  estimate_type = "numeric") |>
    dplyr::mutate(
      strata_name = "overall",
      strata_level = "overall"
    )

  # Add strata estimates if required
  if (!is.null(variables)) {
    cli::cli_progress_bar(
      total = length(variables),
      format = " -- Getting estimates for {cli::pb_bar} {cli::pb_current} of {cli::pb_total} strata"
    )
    for (i in seq_along(variables)) {
      cli::cli_progress_update()
      # Get formula for the model
      name <- variables[[i]]
      uniqueVals <- survData |>
        dplyr::select(.env$name) |>
        tidyr::unite(col = "vars") |>
        dplyr::distinct() |>
        dplyr::pull()
      if(length(uniqueVals) == 1){
        cli::cli_inform("All values of {name} are equal to {uniqueVals}
                        and stratified results will not be returned for
                        this variable")
      } else {
        expr <- stats::as.formula(paste(c(
          "survival::Surv(outcome_or_competing_time, outcome_or_competing_status) ~ 1",
          name
        ), collapse = " + "))
        fit <- survival::survfit(
          formula = expr,
          data = survData |>
            dplyr::filter(dplyr::if_any(.env$name, ~ !is.na(.x)))
        )
        summ <- summary(fit, times = times, extend = TRUE, data.frame = TRUE)
        tidyFit <- broom::tidy(fit)

        maxTimes <- tidyFit |>
          dplyr::group_by(.data$strata, .data$state) |>
          dplyr::summarise(max_time = max(.data$time, na.rm = TRUE))

        fitSummary[[i + 1]] <- as.data.frame(summary(fit, rmean = restrictedMeanFollowUp)$table) |>
          dplyr::rename(
            "number_records" = "n",
            "n_events" = "nevent",
            "restricted_mean_survival" = "rmean",
            "restricted_mean_survival_se" = "se(rmean)"
          ) |>
          dplyr::mutate(analysis_type = "competing_risk") |>
          tibble::rownames_to_column(var = "rowname") |>
          tidyr::separate_wider_delim("rowname",
                                      delim = ", ",
                                      names = c(variables[[i]], "state")) |>
          tidyr::unite(col = "strata", variables[[i]], sep = ", ") |>
          dplyr::left_join(maxTimes,
                           by = c("strata", "state")) |>
          dplyr::rename("outcome" = "state") |>
          dplyr::mutate(
            strata_name = paste(name, collapse = " &&& "),
            strata_level = gsub(", ", " &&& ", gsub(
              paste(paste0(name, "="),
                    collapse = "|"
              ), "",
              .data$strata
            ))
          )  |>
          dplyr::mutate(
            strata_level = gsub("and ([^and ]*)$", "", .data$strata_level)
          ) |>
          dplyr::mutate(strata_level = gsub("[[:space:]]*$", "", .data$strata_level)) |>
          dplyr::mutate(outcome = dplyr::if_else(.data$outcome == "(s0)", "none",
                                                 dplyr::if_else(.data$outcome == "1",
                                                                "outcome", "competing_outcome"
                                                 )
          )) |>
          dplyr::mutate(
            strata = sub(",([^,]*)$", "", .data$strata)
          )

        estimates[[i + 1]] <-  summ |>
          dplyr::rename(
            "estimate" = "pstate",
            "estimate_95CI_lower" = "lower",
            "estimate_95CI_upper" = "upper",
            "outcome" = "state",
            "n_risk" = "n.risk",
            "n_event" = "n.event",
            "n_censor" = "n.censor"
          ) |>
          dplyr::mutate(outcome = dplyr::if_else(.data$outcome == 1,
                                                 "outcome", dplyr::if_else(.data$outcome == 2,
                                                                           "competing_outcome", "none")),
                        analysis_type = "competing_risk",
                        estimate_type = "numeric",
                        strata_name = paste(name, collapse = " &&& "),
                        strata_level = .data$strata) # to use in below join

        for (j in seq_along(name)) {
          estimates[[i + 1]] <- estimates[[i + 1]] |>
            dplyr::mutate(strata_level = stringr::str_replace(
              string = .data$strata_level,
              pattern = paste0(name[j], "="), replacement = ""
            )) |>
            dplyr::mutate(strata_level = stringr::str_replace(
              string = .data$strata_level,
              pattern = ",",
              replacement = " &&&"
            ))
        }

        # keep estimated probabilities only up to the end of follow up for that group
        maxTimePoints <- fitSummary[[i + 1]] |>
          dplyr::select("strata_level", "outcome", "max_time")

        estimates[[i + 1]] <- estimates[[i + 1]] |>
          dplyr::left_join(maxTimePoints,
                           by = c("strata_level", "outcome")
          ) |>
          dplyr::filter(.data$time <= .data$max_time) |>
          dplyr::select(!c("max_time", "strata"))

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] |>
          dplyr::select(!c("max_time", "strata"))
      }}
    cli::cli_progress_done()
  }

  # Output as tibble
  estimates <- dplyr::bind_rows(estimates) |>
    dplyr::as_tibble() |>
    dplyr::select(-"std.err")

  # Helper function to calculate events for a specific eventGap
  calculate_number_events <- function(data, gap) {
    data |>
      dplyr::select("time", "n_event", "outcome", "strata_name", "strata_level") |>
      dplyr::filter(.data$outcome != "none") |>
      dplyr::left_join(
        data |>
          dplyr::filter(.data$outcome == "none") |>
          dplyr::select("time", "n_risk", "n_censor", "strata_name", "strata_level"),
        by = c("time", "strata_name", "strata_level")
      ) |>
      dplyr::mutate(eventGap = gap) |>
      dplyr::group_by(.data$strata_name, .data$strata_level, .data$outcome) |>
      dplyr::mutate(
        n_events = cumsum(.data$n_event),
        n_censor = cumsum(.data$n_censor)
      ) |>
      dplyr::filter(.data$time %% gap == 0 | .data$time == max(.data$time)) |>
      dplyr::mutate(
        n_events = c(.data$n_events[1], diff(.data$n_events)),
        n_censor = c(.data$n_censor[1], diff(.data$n_censor))
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-"n_event")
  }

  # Apply the function for all eventGaps and combine results
  number_events <- purrr::map_dfr(eventGap, ~calculate_number_events(estimates, .x))

  estimates <- estimates |>
    dplyr::select(-c("n_event")) |>
    dplyr::filter(.data$outcome != "none")

  attr(estimates, "events") <- number_events
  attr(estimates, "summary") <- dplyr::as_tibble(dplyr::bind_rows(fitSummary))

  return(estimates)
}

addCohortDetails <- function(x,
                             cdm,
                             targetCohortId,
                             targetCohortTable,
                             outcomeCohortId,
                             outcomeCohortTable,
                             competingOutcomeCohortId,
                             competingOutcomeCohortTable = NULL,
                             resultType) {
  outcomeCohortName <- omopgenerics::settings(cdm[[outcomeCohortTable]]) |>
    dplyr::filter(.data$cohort_definition_id ==
                    .env$outcomeCohortId) |>
    dplyr::pull("cohort_name")

  x <- x |>
    dplyr::mutate(
      cdm_name = attr(cdm, "cdm_name"),
      package_name = "CohortSurvival",
      package_version = as.character(utils::packageVersion("CohortSurvival")),
      result_type = .env$resultType,
      group_name = "target_cohort",
      group_level =
        omopgenerics::settings(cdm[[targetCohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id ==
                        .env$targetCohortId) |>
        dplyr::pull("cohort_name"),
      variable_type = NA,
    )

  if (!is.null(competingOutcomeCohortTable)) {
    competingOutcomeCohortName <- omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) |>
      dplyr::filter(.data$cohort_definition_id ==
                      .env$competingOutcomeCohortId) |>
      dplyr::pull("cohort_name")

    if (competingOutcomeCohortName == outcomeCohortName) {
      competingOutcomeCohortName <- paste0(competingOutcomeCohortName, "_competing_outcome")
    }
    x <- x |>
      dplyr::mutate(outcome = dplyr::if_else(
        .data$outcome == "outcome", outcomeCohortName, competingOutcomeCohortName
      ))
    x <- x |>
      dplyr::mutate(analysis_type = "competing_risk")
  } else {
    x <- x |>
      dplyr::mutate(outcome = dplyr::if_else(
        .data$outcome == "outcome", outcomeCohortName, "no_competing_outcome"
      ))
    x <- x |>
      dplyr::mutate(analysis_type = "single_event")
  }

  return(x)
}

empty_estimates <- function() {
  dplyr::tibble()
}

# Helper function to get cohort id
getCohortId <- function(cohortTable, cdm) {
  omopgenerics::assertTable(cdm[[cohortTable]])
  cohortId <- omopgenerics::cohortCount(cdm[[cohortTable]]) |>
    dplyr::filter(.data$number_records > 0) |>
    dplyr::pull("cohort_definition_id")
  return(cohortId)
}
