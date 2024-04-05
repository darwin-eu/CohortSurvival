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
#' @param targetCohortId targetCohortId
#' @param outcomeCohortTable The outcome cohort table of interest.
#' @param outcomeCohortId ID of event cohorts to include. Only one outcome
#' (and so one ID) can be considered.
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
#' @param minimumSurvivalDays Minimum number of days required for the main cohort
#' to have survived
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#' @param returnParticipants Either TRUE or FALSE. If TRUE, references to
#' participants from the analysis will be returned allowing for further
#' analysis.
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
                                        targetCohortId = NULL,
                                        outcomeCohortTable,
                                        outcomeCohortId = NULL,
                                        outcomeDateVariable = "cohort_start_date",
                                        outcomeWashout = Inf,
                                        censorOnCohortExit = FALSE,
                                        censorOnDate = NULL,
                                        followUpDays = Inf,
                                        strata = NULL,
                                        eventGap = 30,
                                        estimateGap = 1,
                                        minimumSurvivalDays = 1,
                                        minCellCount = 5,
                                        returnParticipants = FALSE) {
  if (is.null(targetCohortId)) {
    CDMConnector::assertTables(cdm, targetCohortTable)
    targetCohortId <- CDMConnector::cohort_count(cdm[[targetCohortTable]]) %>%
      dplyr::filter(.data$number_records > 0) %>%
      dplyr::pull("cohort_definition_id")
  }
  if (is.null(outcomeCohortId)) {
    CDMConnector::assertTables(cdm, outcomeCohortTable)
    outcomeCohortId <- CDMConnector::cohort_count(cdm[[outcomeCohortTable]]) %>%
      dplyr::filter(.data$number_records >0) %>%
      dplyr::pull("cohort_definition_id")
  }

  surv <- list()
  events <- list()
  attrition <- list()
  summary <- list()
  participants <- list()
  for (i in seq_along(targetCohortId)) {
    working_target_id <- targetCohortId[i]
    working_target <- omopgenerics::settings(cdm[[targetCohortTable]]) %>%
      dplyr::filter(.data$cohort_definition_id == working_target_id) %>%
      dplyr::pull("cohort_name")
    for (j in seq_along(outcomeCohortId)) {
      working_outcome_id <- outcomeCohortId[j]
      working_outcome <- omopgenerics::settings(cdm[[outcomeCohortTable]]) %>%
        dplyr::filter(.data$cohort_definition_id == working_outcome_id) %>%
        dplyr::pull("cohort_name")
      cli::cli_inform("- Getting survival for target cohort '{working_target}'
                    and outcome cohort '{working_outcome}'")

      surv[[paste0(i, "_", j)]] <- estimateSurvival(
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
        minimumSurvivalDays = minimumSurvivalDays,
        minCellCount = minCellCount,
        returnParticipants = returnParticipants
      )
      attrition[[paste0(i, "_", j)]] <- attr(surv[[paste0(i, "_", j)]], "attrition") %>%
        dplyr::mutate(
          exposure_id = i,
          outcome_id = j
        )
      events[[paste0(i, "_", j)]] <- attr(surv[[paste0(i, "_", j)]], 'events')
      summary[[paste0(i, "_", j)]] <- attr(surv[[paste0(i, "_", j)]], 'summary')
      if(returnParticipants) {
        participants[[paste0(i, "_", j)]] <- attr(surv[[paste0(i, "_", j)]], "participants") %>%
          dplyr::mutate(
            exposure_id = i,
            outcome_id = j
          ) %>%
          dplyr::collect()
      }
    }
  }

  surv_estimates <- dplyr::bind_rows(surv)
  # add attributes

  events <- dplyr::bind_rows(events)
  attr(surv_estimates, "attrition") <- dplyr::bind_rows(attrition)
  summary <- dplyr::bind_rows(summary)

  surv_estimates <- dplyr::bind_rows(
    surv_estimates,
    events,
    summary
  ) %>%
    dplyr::mutate(estimate_value = as.character(.data$estimate_value),
                  result_id = 1L)

  if(returnParticipants) {
    attr(surv_estimates, 'participants') <- dplyr::bind_rows(participants)
  }

  attr(surv_estimates, 'events') <- NULL
  attr(surv_estimates, 'summary') <- NULL


  surv_estimates <- omopgenerics::newSummarisedResult(surv_estimates)

  return(surv_estimates)
}

#' Estimate survival for a given event and competing risk of interest using
#' cohorts in the OMOP Common Data Model
#'
#' @param cdm CDM reference
#' @param targetCohortTable targetCohortTable
#' @param targetCohortId targetCohortId
#' @param outcomeCohortTable The outcome cohort table of interest.
#' @param outcomeCohortId ID of event cohorts to include. Only one outcome
#' (and so one ID) can be considered.
#' @param outcomeDateVariable  Variable containing date of outcome event
#' @param outcomeWashout Washout time in days for the outcome
#' @param competingOutcomeCohortTable The competing outcome cohort table of interest.
#' @param competingOutcomeCohortId ID of event cohorts to include. Only one competing outcome
#' (and so one ID) can be considered.
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
#' @param minimumSurvivalDays Minimum number of days required for the main cohort
#' to have survived
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#' @param returnParticipants Either TRUE or FALSE. If TRUE, references to
#' participants from the analysis will be returned allowing for further
#' analysis.
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
                                          targetCohortId = NULL,
                                          outcomeCohortTable,
                                          outcomeCohortId = NULL,
                                          outcomeDateVariable = "cohort_start_date",
                                          outcomeWashout = Inf,
                                          competingOutcomeCohortTable,
                                          competingOutcomeCohortId = NULL,
                                          competingOutcomeDateVariable = "cohort_start_date",
                                          competingOutcomeWashout = Inf,
                                          censorOnCohortExit = FALSE,
                                          censorOnDate = NULL,
                                          followUpDays = Inf,
                                          strata = NULL,
                                          eventGap = 30,
                                          estimateGap = 1,
                                          minimumSurvivalDays = 1,
                                          minCellCount = 5,
                                          returnParticipants = FALSE) {
  if (is.null(targetCohortId)) {
    CDMConnector::assertTables(cdm, targetCohortTable)
    targetCohortId <- CDMConnector::cohort_count(cdm[[targetCohortTable]]) %>%
      dplyr::filter(.data$number_records >0) %>%
      dplyr::pull("cohort_definition_id")
  }
  if (is.null(outcomeCohortId)) {
    CDMConnector::assertTables(cdm, outcomeCohortTable)
    outcomeCohortId <- CDMConnector::cohort_count(cdm[[outcomeCohortTable]]) %>%
      dplyr::filter(.data$number_records >0) %>%
      dplyr::pull("cohort_definition_id")
  }
  if (is.null(competingOutcomeCohortId)) {
    CDMConnector::assertTables(cdm, competingOutcomeCohortTable)
    competingOutcomeCohortId <- CDMConnector::cohort_count(cdm[[competingOutcomeCohortTable]])  %>%
      dplyr::filter(.data$number_records >0) %>%
      dplyr::pull("cohort_definition_id")
  }

  surv <- list()
  attrition <- list()
  events <- list()
  summary <- list()
  participants <- list()
  for (i in seq_along(targetCohortId)) {
    working_target_id <- targetCohortId[i]
    working_target <- omopgenerics::settings(cdm[[targetCohortTable]]) %>%
      dplyr::filter(.data$cohort_definition_id == working_target_id) %>%
      dplyr::pull("cohort_name")
    for (j in seq_along(outcomeCohortId)) {
      working_outcome_id <- outcomeCohortId[j]
      working_outcome <- omopgenerics::settings(cdm[[outcomeCohortTable]]) %>%
        dplyr::filter(.data$cohort_definition_id == working_outcome_id) %>%
        dplyr::pull("cohort_name")
      for (k in seq_along(competingOutcomeCohortId)) {
        working_competing_outcome_id <- competingOutcomeCohortId[k]
        working_competing_outcome <- omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) %>%
          dplyr::filter(.data$cohort_definition_id == working_competing_outcome_id) %>%
          dplyr::pull("cohort_name")

        cli::cli_inform("- Getting survival for target cohort '{working_target}'
                    and outcome cohort '{working_outcome}' with
                    competing outcome cohort '{working_competing_outcome}'")
        surv[[paste0(i, "_", j, "_", k)]] <- estimateSurvival(
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
          minimumSurvivalDays = minimumSurvivalDays,
          minCellCount = minCellCount,
          returnParticipants = returnParticipants
        )
        if(length(surv[[paste0(i, "_", j, "_", k)]]) > 0) {
          attrition[[paste0(i, "_", j, "_", k)]] <- attr(surv[[paste0(i, "_", j, "_", k)]], "attrition") %>%
            dplyr::mutate(
              exposure_id = i,
              outcome_id = j,
              competing_outcome_id = k
            )
          events[[paste0(i, "_", j, "_", k)]] <- attr(surv[[paste0(i, "_", j, "_", k)]], 'events')
          summary[[paste0(i, "_", j, "_", k)]] <- attr(surv[[paste0(i, "_", j, "_", k)]], 'summary')
          if(returnParticipants) {
            participants[[paste0(i, "_", j, "_", k)]] <- attr(surv[[paste0(i, "_", j, "_", k)]], "participants") %>%
              dplyr::mutate(
                exposure_id = i,
                outcome_id = j,
                competing_outcome_id = k
              ) %>%
              dplyr::collect()
          }
        }
      }
    }
  }

  # Remove empty elements for analysis which have no output
  surv[lengths(surv) == 0] <- NULL

  surv_estimates <- dplyr::bind_rows(surv)
  # add attributes

  events <- dplyr::bind_rows(events)
  attr(surv_estimates, 'attrition') <- dplyr::bind_rows(attrition)
  summary <- dplyr::bind_rows(summary)

  if(returnParticipants) {
    attr(surv_estimates, 'participants') <- dplyr::bind_rows(participants)
  }

  surv_estimates <- dplyr::bind_rows(
    surv_estimates,
    events,
    summary
  ) %>%
    dplyr::mutate(estimate_value = as.character(.data$estimate_value),
                  result_id = 1L)

  attr(surv_estimates, 'events') <- NULL
  attr(surv_estimates, 'summary') <- NULL

  surv_estimates <- omopgenerics::newSummarisedResult(surv_estimates)

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
                             competingOutcomeCohortId = NULL,
                             competingOutcomeDateVariable = "cohort_start_date",
                             competingOutcomeWashout = Inf,
                             censorOnCohortExit = FALSE,
                             censorOnDate = NULL,
                             followUpDays = Inf,
                             strata = NULL,
                             eventGap = 30,
                             estimateGap = 1,
                             minimumSurvivalDays = 1,
                             minCellCount = 5,
                             returnParticipants = FALSE) {
  # check input
  errorMessage <- checkmate::makeAssertCollection()

  checkmate::assertCharacter(targetCohortTable,
                             len = 1,
                             add = errorMessage
  )
  checkmate::assertCharacter(outcomeCohortTable,
                             len = 1,
                             add = errorMessage
  )
  checkmate::assertCharacter(competingOutcomeCohortTable,
                             len = 1,
                             null.ok = TRUE,
                             add = errorMessage
  )
  checkCdm(cdm, tables = c(
    "person", "observation_period",
    targetCohortTable,
    outcomeCohortTable
  ))

  checkIsCohort_exp(cdm[[targetCohortTable]])
  checkmate::assertIntegerish(targetCohortId,
                              len = 1,
                              lower = 1,
                              add = errorMessage
  )
  checkStrata(strata, cdm[[targetCohortTable]])
  checkmate::assertIntegerish(outcomeCohortId,
                              len = 1,
                              lower = 1,
                              add = errorMessage
  )
  checkmate::assertIntegerish(competingOutcomeCohortId,
                              len = 1,
                              lower = 1,
                              add = errorMessage
  )

  checkmate::assertCharacter(outcomeDateVariable,
                             len = 1,
                             add = errorMessage
  )
  checkmate::assertCharacter(competingOutcomeDateVariable,
                             len = 1,
                             add = errorMessage
  )
  checkmate::assertLogical(censorOnCohortExit,
                           len = 1,
                           add = errorMessage
  )
  if (!is.null(censorOnDate)) {
    checkdate <- censorOnDate %>% inherits("Date")
    if (!checkdate) {
      cli::cli_abort("{censorOnDate} is neither NULL nor of type Date")
    }
  }
  if (followUpDays != "Inf") {
    checkmate::assertIntegerish(followUpDays,
                                len = 1,
                                lower = 0,
                                add = errorMessage
    )
  }
  if (outcomeWashout != "Inf") {
    checkmate::assertIntegerish(outcomeWashout,
                                len = 1,
                                lower = 1,
                                add = errorMessage
    )
  }
  if (competingOutcomeWashout != "Inf") {
    checkmate::assertIntegerish(competingOutcomeWashout,
                                len = 1,
                                lower = 0,
                                add = errorMessage
    )
  }
  checkmate::assertIntegerish(eventGap,
                              lower = 1,
                              add = errorMessage
  )
  checkmate::assertIntegerish(estimateGap,
                              lower = 0,
                              add = errorMessage
  )
  checkmate::assertIntegerish(minimumSurvivalDays,
                              len = 1,
                              lower = 0,
                              add = errorMessage
  )
  checkmate::assertIntegerish(minCellCount,
                              len = 1,
                              lower = 0,
                              null.ok = FALSE,
                              add = errorMessage
  )
  checkmate::assertLogical(returnParticipants,
                           len = 1,
                           add = errorMessage
  )

  checkmate::reportAssertions(collection = errorMessage)

  workingExposureTable <- cdm[[targetCohortTable]] %>%
    dplyr::filter(.data$cohort_definition_id == .env$targetCohortId)

  attrition <- recordAttrition(
    table = workingExposureTable,
    id = "subject_id",
    reasonId = 1,
    reason = "Starting analysis population"
  )

  # addCohortSurvival for primary event of interest
  workingExposureTable <- workingExposureTable %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = outcomeCohortTable,
      outcomeCohortId = outcomeCohortId,
      outcomeDateVariable = outcomeDateVariable,
      outcomeWashout = outcomeWashout,
      censorOnCohortExit = censorOnCohortExit,
      censorOnDate = censorOnDate,
      followUpDays = followUpDays
    ) %>%
    dplyr::rename(
      "outcome_time" = "time",
      "outcome_status" = "status"
    )

  # competing risk (if there is one)
  if (!is.null(competingOutcomeCohortTable)) {
    workingExposureTable <- workingExposureTable %>%
      addCohortSurvival(
        cdm = cdm,
        outcomeCohortTable = competingOutcomeCohortTable,
        outcomeCohortId = competingOutcomeCohortId,
        outcomeDateVariable = competingOutcomeDateVariable,
        outcomeWashout = competingOutcomeWashout,
        censorOnCohortExit = censorOnCohortExit,
        censorOnDate = censorOnDate,
        followUpDays = followUpDays
      ) %>%
      dplyr::rename(
        "competing_risk_time" = "time",
        "competing_risk_status" = "status"
      )
  }

  # collect
  survDataDb <- workingExposureTable %>%
    dplyr::filter(!is.na(.data$outcome_time) &&
                    !is.na(.data$outcome_status))

  survData <- survDataDb %>%
    dplyr::collect()

  attrition <- recordAttrition(
    table = survData,
    id = "subject_id",
    reasonId = 2,
    reason = "Outcome status not NA",
    existingAttrition = attrition
  )

  survData <- survData %>%
    dplyr::filter(.data$outcome_time >= .env$minimumSurvivalDays)

  attrition <- recordAttrition(
    table = survData,
    id = "subject_id",
    reasonId = 3,
    reason = paste0("Survival days for outcome less than ", minimumSurvivalDays),
    existingAttrition = attrition
  )

  if (!is.null(competingOutcomeCohortTable)) {
    survData <- survData %>%
      dplyr::filter(.data$competing_risk_time >= .env$minimumSurvivalDays)

    attrition <- recordAttrition(
      table = survData,
      id = "subject_id",
      reasonId = 4,
      reason = paste0("Survival days for competing outcome less than ", minimumSurvivalDays),
      existingAttrition = attrition
    )
  }

  if (!is.null(competingOutcomeCohortTable)) {
    # - add competing risk variable
    # 0: no event, 2: outcome event, 3: competing risk event
    survData <- addCompetingRiskVars(
      data = survData,
      time1 = "outcome_time",
      status1 = "outcome_status",
      time2 = "competing_risk_time",
      status2 = "competing_risk_status",
      nameOutTime = "outcome_or_competing_time",
      nameOutStatus = "outcome_or_competing_status"
    )
  }

  # time points to extract survival estimates
  if(followUpDays == "Inf") {
    timepoints <- seq(0, max(survData$outcome_time), by = estimateGap)
  } else {
    timepoints <- seq(0, max(survData$outcome_time, followUpDays), by = estimateGap)
  }

  # fit survival, with strata
  if (is.null(competingOutcomeCohortTable)) {
    surv <- singleEventSurvival(
      survData = survData,
      times = timepoints,
      variables = strata,
      eventGap = eventGap
    )
  } else {
    surv <- competingRiskSurvival(
      survData = survData,
      times = timepoints,
      variables = strata,
      eventGap = eventGap
    )
  }

  if (nrow(surv) > 0) {
    survivalEstimates <- addCohortDetails(
      x = surv,
      cdm = cdm,
      targetCohortId = targetCohortId,
      targetCohortTable = targetCohortTable,
      outcomeCohortId = outcomeCohortId,
      outcomeCohortTable = outcomeCohortTable,
      competingOutcomeCohortId = competingOutcomeCohortId,
      competingOutcomeCohortTable = competingOutcomeCohortTable
    )

    survivalEstimates <- survivalEstimates %>%
      tidyr::pivot_longer(
        cols = "outcome",
        names_to = "variable_name",
        values_to = "variable_level"
      )
      survivalEstimates <- survivalEstimates %>%
        dplyr::mutate(variable_name = dplyr::if_else(
          .data$analysis_type == "competing_risk",
          "cumulative_failure_probability",
          "survival_probability"))

    survivalEstimates <- survivalEstimates %>%
      dplyr::select(!c("n_risk","variable_type")) %>%
      tidyr::pivot_longer(
        cols = c(
          "estimate",
          "estimate_95CI_lower",
          "estimate_95CI_upper"
        ),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) %>%
      dplyr::mutate(
        outcome = omopgenerics::settings(cdm[[outcomeCohortTable]]) %>%
          dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
          dplyr::pull("cohort_name")
      )

    if(!is.null(competingOutcomeCohortTable)) {
      survivalEstimates <- survivalEstimates %>%
        dplyr::mutate(competing_outcome = omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) %>%
                        dplyr::filter(.data$cohort_definition_id == .env$competingOutcomeCohortId) %>%
                        dplyr::pull("cohort_name")) %>%
        uniteNameLevel(cols = c("time", "analysis_type", "outcome", "competing_outcome"),
                       name = "additional_name", level = "additional_level")
    } else {
      survivalEstimates <- survivalEstimates %>%
        uniteNameLevel(cols = c("time", "analysis_type", "outcome"),
                       name = "additional_name", level = "additional_level")
    }

    survivalEstimates <- var_order(survivalEstimates) %>%
      dplyr::distinct()

    # add attributes
    if (isTRUE(returnParticipants)) {
      participantsRef <- survDataDb %>%
        dplyr::select(
          "cohort_definition_id",
          "subject_id",
          "cohort_start_date",
          "cohort_end_date"
        ) %>%
        dplyr::compute()

      attr(participantsRef, "cohort_set") <- participantsRef %>%
        dplyr::select("cohort_definition_id") %>%
        dplyr::distinct() %>%
        dplyr::mutate(cohort_name = paste0(
          "survival_participants_",
          as.integer(.data$cohort_definition_id)
        )) %>%
        dplyr::collect()

      attr(participantsRef, "cohort_attrition") <- participantsRef %>%
        dplyr::group_by(.data$cohort_definition_id) %>%
        dplyr::summarise(
          number_records = dplyr::n(),
          number_subjects = dplyr::n_distinct(.data$subject_id),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          "reason_id" = 1,
          "reason" = "Initial qualifying events",
          "excluded_records" = 0,
          "excluded_subjects" = 0
        ) %>%
        dplyr::collect()

      attr(participantsRef, "tbl_name") <- "survival_participants"

      attr(survivalEstimates, "participants") <- omopgenerics::newCohortTable(
        participantsRef
      )
    }

    events <- attr(surv, "events") %>%
      dplyr::group_by(.data$eventGap, .data$strata_name, .data$strata_level, .data$outcome) %>%
      dplyr::mutate(to_suppress = dplyr::if_else(.data$n_risk < .env$minCellCount,
                                                 1, 0)) %>%
      dplyr::mutate(to_suppress = cumsum(.data$to_suppress)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$to_suppress == 0) %>%
      dplyr::select(!"to_suppress")

    events <- events %>%
      dplyr::mutate(n_events = dplyr::if_else(.data$n_events < 5 & .data$n_events >0,
                                              NA, .data$n_events))

    attr(survivalEstimates, "events") <- addCohortDetails(
      x = events,
      cdm = cdm,
      targetCohortId = targetCohortId,
      targetCohortTable = targetCohortTable,
      outcomeCohortId = outcomeCohortId,
      outcomeCohortTable = outcomeCohortTable,
      competingOutcomeCohortTable = competingOutcomeCohortTable,
      competingOutcomeCohortId = competingOutcomeCohortId) %>%
      dplyr::select(!"variable_type") %>%
      tidyr::pivot_longer(cols = c("n_risk", "n_events"),
                          names_to = "estimate_name",
                          values_to = "estimate_value") %>%
      dplyr::mutate(
        variable_name = "survival_events",
        estimate_type = "numeric",
        result_type = "survival_events"
      ) %>%
      dplyr::rename(variable_level = "outcome") %>%
      dplyr::mutate(outcome = omopgenerics::settings(cdm[[outcomeCohortTable]]) %>%
                      dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
                      dplyr::pull("cohort_name")) %>%
      dplyr::rename("eventgap" = "eventGap")

    if(!is.null(competingOutcomeCohortTable)) {
      attr(survivalEstimates, "events") <- attr(survivalEstimates, "events") %>%
          dplyr::mutate(competing_outcome = omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) %>%
                          dplyr::filter(.data$cohort_definition_id == .env$competingOutcomeCohortId) %>%
                          dplyr::pull("cohort_name")) %>%
        uniteNameLevel(cols = c("time", "analysis_type", "eventgap", 'outcome', 'competing_outcome'),
                       name = "additional_name", level = "additional_level") %>%
        var_order()
    } else {
      attr(survivalEstimates, "events") <- attr(survivalEstimates, "events") %>%
        uniteNameLevel(cols = c("time", "analysis_type", "eventgap", 'outcome'),
                       name = "additional_name", level = "additional_level") %>%
        var_order()
    }

    attr(survivalEstimates, "attrition") <- attrition

    if (is.null(competingOutcomeCohortTable)) {
      attr(survivalEstimates, "summary") <- addCohortDetails(
        x = attr(surv, "summary"),
        cdm = cdm,
        targetCohortId = targetCohortId,
        targetCohortTable = targetCohortTable,
        outcomeCohortId = outcomeCohortId,
        outcomeCohortTable = outcomeCohortTable,
        summary = TRUE
      ) %>%
        dplyr::mutate(analysis_type = "single_event")
    } else {
      attr(surv, "summary") <- attr(surv, "summary") %>%
        dplyr::filter(.data$outcome != "none")

      attr(survivalEstimates, "summary") <- addCohortDetails(
        x = attr(surv, "summary"),
        cdm = cdm,
        targetCohortId = targetCohortId,
        targetCohortTable = targetCohortTable,
        outcomeCohortId = outcomeCohortId,
        outcomeCohortTable = outcomeCohortTable,
        competingOutcomeCohortTable = competingOutcomeCohortTable,
        competingOutcomeCohortId = competingOutcomeCohortId,
        summary = TRUE
      ) %>%
        dplyr::mutate(analysis_type = "competing_risk")
    }

    attr(survivalEstimates, "summary") <- attr(survivalEstimates, "summary") %>%
      dplyr::filter(.data$outcome != "none") %>%
      dplyr::filter(.data$number_records >= .env$minCellCount) %>%
      dplyr::mutate(
        result_type = "survival_summary",
        variable_name = "survival_summary",
        variable_level = .data$outcome,
        estimate_type = "numeric"
      ) %>%
      dplyr::select(!c("variable_type", "outcome")) %>%
      tidyr::pivot_longer(
        cols = -c(
          "cdm_name",
          "package_name",
          "package_version",
          "result_type",
          "group_name",
          "group_level",
          "strata_name",
          "strata_level",
          "variable_name",
          "variable_level",
          "estimate_type",
          "analysis_type"
        ),
        names_to = "estimate_name",
        values_to = "estimate_value"
      ) %>%
      var_order() %>%
      dplyr::mutate(estimate_value = round(.data$estimate_value)) %>%
      dplyr::relocate("analysis_type",
                      .after = "estimate_name"
      ) %>%
      dplyr::relocate("estimate_value",
                      .after = "analysis_type"
      ) %>%
      dplyr::mutate(outcome = omopgenerics::settings(cdm[[outcomeCohortTable]]) %>%
                      dplyr::filter(.data$cohort_definition_id == .env$outcomeCohortId) %>%
                      dplyr::pull("cohort_name"))

    if(!is.null(competingOutcomeCohortTable)) {
      attr(survivalEstimates, "summary") <- attr(survivalEstimates, "summary") %>%
        dplyr::mutate(competing_outcome = omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) %>%
                        dplyr::filter(.data$cohort_definition_id == .env$competingOutcomeCohortId) %>%
                        dplyr::pull("cohort_name")) %>%
        uniteNameLevel(cols = c('analysis_type', 'outcome', 'competing_outcome'),
                       name = "additional_name", level = "additional_level") %>%
        var_order()
    } else {
      attr(survivalEstimates, "summary") <- attr(survivalEstimates, "summary") %>%
        uniteNameLevel(cols = c("analysis_type", 'outcome'),
                       name = "additional_name", level = "additional_level") %>%
        var_order()
    }

    # round estimates
    survivalEstimates <- survivalEstimates %>%
      dplyr::mutate(estimate_value = round(.data$estimate_value, 4))

    # obscure counts below minCellCount
    survivalEstimates <- suppressSurvivalCounts(survivalEstimates, minCellCount)
  } else {
    survivalEstimates <- surv
  }


  return(survivalEstimates)
}

addCompetingRiskVars <- function(data, time1, status1,
                                 time2, status2,
                                 nameOutTime,
                                 nameOutStatus) {
  # - add competing risk variables (time and status)
  # 0: no event, 1: event 1, 2: event 2
  data <- data %>%
    dplyr::mutate(!!nameOutTime := dplyr::if_else(
      .data[[time2]] > .data[[time1]],
      .data[[time1]], .data[[time2]]
    )) %>%
    dplyr::mutate(!!nameOutStatus := as.factor(dplyr::if_else(
      .data[[time2]] <= .data[[time1]],
      2 * .data[[status2]], .data[[status1]]
    )))

  return(data)
}

singleEventSurvival <- function(survData, times, variables, eventGap) {
  estimates <- list()
  fitSummary <- list()

  var_columns <- unlist(variables) %>% unique()

  cli::cli_progress_message("Getting overall estimates")
  fit <- survival::survfit(survival::Surv(outcome_time, outcome_status) ~ 1,
                           data = survData
  )

  q0 <- stats::quantile(fit, probs = 0)
  q25 <- stats::quantile(fit, probs = 0.25)
  q75 <- stats::quantile(fit, probs = 0.75)
  q100 <- stats::quantile(fit, probs = 1)

  fitSummary[[1]] <- as.data.frame(t(summary(fit)$table)) %>%
    dplyr::select(!dplyr::any_of(c("rmean", "se(rmean)", "n.max", "n.start"))) %>%
    dplyr::rename(
      "number_records" = "records",
      "median_survival" = "median",
      "median_survival_95CI_lower" = "0.95LCL",
      "median_survival_95CI_higher" = "0.95UCL"
    ) %>%
    dplyr::mutate(
      q0_survival = .env$q0$quantile,
      q0_survival_95CI_lower = .env$q0$lower,
      q0_survival_95CI_higher = .env$q0$upper,
      q25_survival = .env$q25$quantile,
      q25_survival_95CI_lower = .env$q25$lower,
      q25_survival_95CI_higher = .env$q25$upper,
      q75_survival = .env$q75$quantile,
      q75_survival_95CI_lower = .env$q75$lower,
      q75_survival_95CI_higher = .env$q75$upper,
      q100_survival = .env$q100$quantile,
      q100_survival_95CI_lower = .env$q100$lower,
      q100_survival_95CI_higher = .env$q100$upper
    ) %>%
    dplyr::mutate(analysis_type = "single event") %>%
    dplyr::mutate(
      strata_name = "overall",
      strata_level = "overall",
      outcome = "outcome"
    )

  summ <- summary(fit, times = times, extend = TRUE)
  estimates[[1]] <- dplyr::bind_rows(
    dplyr::tibble(
      outcome = "outcome",
      time = summ$time,
      n_event = summ$n.event,
      n_risk = summ$n.risk,
      estimate_type = "numeric",
      estimate = summ$surv,
      estimate_95CI_lower = summ$lower,
      estimate_95CI_upper = summ$upper
    )
  ) %>%
    dplyr::mutate(
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
      uniqueVals <- survData %>%
        dplyr::select(.env$name) %>%
        tidyr::unite(col = "vars") %>%
        dplyr::distinct() %>%
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
          data.frame(x) %>%
            tibble::rownames_to_column(var = "strata")
        }
        q0 <- stats::quantile(fit, probs = 0)
        q0 <- lapply(q0, format_surv_strata_quantile)
        q25 <- stats::quantile(fit, probs = 0.25)
        q25 <- lapply(q25, format_surv_strata_quantile)
        q75 <- stats::quantile(fit, probs = 0.75)
        q75 <- lapply(q75, format_surv_strata_quantile)
        q100 <- stats::quantile(fit, probs = 1)
        q100 <- lapply(q100, format_surv_strata_quantile)

        fitSummary[[i + 1]] <- as.data.frame(summary(fit)$table) %>%
          dplyr::select(!dplyr::any_of(c("rmean", "se(rmean)", "n.max", "n.start"))) %>%
          dplyr::rename(
            "number_records" = "records",
            "median_survival" = "median",
            "median_survival_95CI_lower" = "0.95LCL",
            "median_survival_95CI_higher" = "0.95UCL"
          ) %>%
          tibble::rownames_to_column(var = "strata")

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] %>%
          dplyr::left_join(q0$quantile, by = "strata") %>%
          dplyr::rename("q0_survival" = "X0") %>%
          dplyr::left_join(q0$lower, by = "strata") %>%
          dplyr::rename("q0_survival_95CI_lower" = "X0") %>%
          dplyr::left_join(q0$upper, by = "strata") %>%
          dplyr::rename("q0_survival_95CI_higher" = "X0") %>%
          dplyr::left_join(q25$quantile, by = "strata") %>%
          dplyr::rename("q25_survival" = "X25") %>%
          dplyr::left_join(q25$lower, by = "strata") %>%
          dplyr::rename("q25_survival_95CI_lower" = "X25") %>%
          dplyr::left_join(q25$upper, by = "strata") %>%
          dplyr::rename("q25_survival_95CI_higher" = "X25") %>%
          dplyr::left_join(q75$quantile, by = "strata") %>%
          dplyr::rename("q75_survival" = "X75") %>%
          dplyr::left_join(q75$lower, by = "strata") %>%
          dplyr::rename("q75_survival_95CI_lower" = "X75") %>%
          dplyr::left_join(q75$upper, by = "strata") %>%
          dplyr::rename("q75_survival_95CI_higher" = "X75") %>%
          dplyr::left_join(q100$quantile, by = "strata") %>%
          dplyr::rename("q100_survival" = "X100") %>%
          dplyr::left_join(q100$lower, by = "strata") %>%
          dplyr::rename("q100_survival_95CI_lower" = "X100") %>%
          dplyr::left_join(q100$upper, by = "strata") %>%
          dplyr::rename("q100_survival_95CI_higher" = "X100")

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] %>%
          dplyr::left_join(
            tidyFit %>%
              dplyr::group_by(.data$strata) %>%
              dplyr::summarise(max_time = max(.data$time, na.rm = TRUE)),
            by = "strata"
          )

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] %>%
          dplyr::mutate(
            analysis_type = "single event",
            outcome = "outcome"
          )

        summ <- summary(fit, times = times, extend = TRUE)
        estimates[[i + 1]] <- dplyr::bind_rows(
          dplyr::tibble(
            strata = summ$strata,
            outcome = "outcome",
            time = summ$time,
            n_event = summ$n.event,
            n_risk = summ$n.risk,
            estimate_type = "numeric",
            estimate = summ$surv,
            estimate_95CI_lower = summ$lower,
            estimate_95CI_upper = summ$upper
          )
        ) %>%
          dplyr::mutate(analysis_type = "single_event")

        # Add strata variable columns in a good format
        for (j in seq_along(name)) {
          name_w <- name
          estimates[[i + 1]] <- estimates[[i + 1]] %>%
            dplyr::mutate(
              strata_name = paste(name_w, collapse = " and "),
              strata_level = rep(gsub(", ", " and ", gsub(
                paste(paste0(name_w, "="),
                      collapse = "|"
                ), "",
                summ$strata
              )), 1) # change to 2 if both probs (K)
            )

          fitSummary[[i + 1]] <- fitSummary[[i + 1]] %>%
            dplyr::mutate(
              strata_name = paste(name_w, collapse = " and "),
              strata_level = gsub(", ", " and ", gsub(
                paste(paste0(name_w, "="),
                      collapse = "|"
                ), "",
                fitSummary[[i + 1]]$strata
              ))
            )
        }

        # keep estimated probabilities only up to the end of follow up for that group
        maxTimePoints <- fitSummary[[i + 1]] %>%
          dplyr::select("strata", "max_time")

        estimates[[i + 1]] <- estimates[[i + 1]] %>%
          dplyr::left_join(maxTimePoints,
                           by = "strata"
          ) %>%
          dplyr::filter(.data$time <= .data$max_time) %>%
          dplyr::select(!c("max_time", "strata"))

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] %>%
          dplyr::select(!c("max_time", "strata"))
      }
    }
    cli::cli_progress_done()
  }

  # Output as tibble
  estimates <- dplyr::bind_rows(estimates)

  # Get number of events for all eventGaps
  number_events <- estimates %>%
    dplyr::filter(.data$estimate_type == "numeric") %>%
    dplyr::group_by(.data$strata_name, .data$strata_level) %>%
    dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
    dplyr::filter(.data$time %% eventGap[1] == 0 | .data$time == max(.data$time)) %>%
    dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      eventGap = eventGap[1],
      outcome = "outcome"
    ) %>%
    dplyr::select(
      "time", "n_risk", "n_events", "eventGap", "outcome",
      "strata_name", "strata_level"
    )

  for (t in eventGap[-1]) {
    number_events <- dplyr::union_all(
      number_events,
      estimates %>%
        dplyr::filter(.data$estimate_type == "numeric") %>%
        dplyr::group_by(.data$strata_name, .data$strata_level) %>%
        dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
        dplyr::filter(.data$time %% t == 0 | .data$time == max(.data$time)) %>%
        dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(eventGap = t) %>%
        dplyr::select(
          "time", "n_risk","n_events", "eventGap", "outcome",
          "strata_name", "strata_level"
        )
    )
  }

  estimates <- estimates %>%
    dplyr::select(!"n_event")

  attr(estimates, "events") <- number_events
  attr(estimates, "summary") <- dplyr::bind_rows(fitSummary)
  row.names(attr(estimates, "summary")) <- NULL
  attr(estimates, "summary") <- dplyr::as_tibble(attr(estimates, "summary"))


  return(estimates)
}

competingRiskSurvival <- function(survData, times, variables, eventGap) {
  if (!length(unique(as.character(survData$outcome_or_competing_status))) == 3) {
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

  var_columns <- unlist(variables) %>% unique()

  cli::cli_progress_message("Getting overall estimates")
  fit <- survival::survfit(
    formula = survival::Surv(
      outcome_or_competing_time,
      outcome_or_competing_status
    ) ~ 1,
    data = survData
  )
  summ <- summary(fit, times = times, extend = TRUE)

  if(ncol(summ$n.event) < 3){
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

  fitSummary[[1]] <- as.data.frame(summary(fit)$table) %>%
    dplyr::select(!dplyr::any_of(c("rmean"))) %>%
    dplyr::rename(
      "number_records" = "n",
      "n_events" = "nevent"
    ) %>%
    dplyr::mutate(analysis_type = "competing_risk") %>%
    dplyr::mutate(
      strata_name = "overall",
      strata_level = "overall"
    ) %>%
    tibble::rownames_to_column(var = "outcome") %>%
    dplyr::mutate(outcome = dplyr::if_else(.data$outcome == "(s0)", "none",
                                           dplyr::if_else(.data$outcome == "1",
                                                          "outcome", "competing_outcome"
                                           )
    ))

  estimates[[1]] <- dplyr::bind_rows(
    dplyr::bind_cols(
      data.frame(
        outcome = 1L,
        time = summ$time,
        n_event = summ$n.event[, 2],
        n_risk = summ$n.risk[, 1],
        estimate_type = "numeric"
      ),
      as.data.frame(summ$pstate) %>%
        dplyr::rename("estimate" = "V2") %>%
        dplyr::select("estimate"),
      as.data.frame(summ$lower) %>%
        dplyr::rename("estimate_95CI_lower" = "V2") %>%
        dplyr::select("estimate_95CI_lower"),
      as.data.frame(summ$upper) %>%
        dplyr::rename("estimate_95CI_upper" = "V2") %>%
        dplyr::select("estimate_95CI_upper")
    ),
    dplyr::bind_cols(
      data.frame(
        outcome = 2L,
        time = summ$time,
        n_event = summ$n.event[, 3],
        n_risk = summ$n.risk[, 1],
        estimate_type = "numeric"
      ),
      as.data.frame(summ$pstate) %>%
        dplyr::rename("estimate" = "V3") %>%
        dplyr::select("estimate"),
      as.data.frame(summ$lower) %>%
        dplyr::rename("estimate_95CI_lower" = "V3") %>%
        dplyr::select("estimate_95CI_lower"),
      as.data.frame(summ$upper) %>%
        dplyr::rename("estimate_95CI_upper" = "V3") %>%
        dplyr::select("estimate_95CI_upper")
    )
  ) %>%
    dplyr::mutate(outcome = dplyr::if_else(.data$outcome == 1,
                                           "outcome",
                                           "competing_outcome"
    ),
    estimate = .data$estimate,
    estimate_95CI_upper = .data$estimate_95CI_upper,
    estimate_95CI_lower = .data$estimate_95CI_lower
    ) %>%
    dplyr::mutate(analysis_type = "competing_risk") %>%
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
      uniqueVals <- survData %>%
        dplyr::select(.env$name) %>%
        tidyr::unite(col = "vars") %>%
        dplyr::distinct() %>%
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
          data = survData %>%
            dplyr::filter(dplyr::if_any(.env$name, ~ !is.na(.x)))
        )
        summ <- summary(fit, times = times, extend = TRUE)
        tidyFit <- broom::tidy(fit)

        fitSummary[[i + 1]] <- as.data.frame(summary(fit)$table) %>%
          dplyr::select(!dplyr::any_of(c("rmean"))) %>%
          dplyr::rename(
            "number_records" = "n",
            "n_events" = "nevent"
          ) %>%
          dplyr::mutate(analysis_type = "competing_risk") %>%
          tibble::rownames_to_column(var = "strata") %>%
          dplyr::mutate(
            strata_name = paste(name, collapse = " and "),
            strata_level = gsub(", ", " and ", gsub(
              paste(paste0(name, "="),
                    collapse = "|"
              ), "",
              .data$strata
            ))
          ) %>%
          dplyr::mutate(
            outcome = gsub("^.*and", "", .data$strata_level)
          ) %>%
          dplyr::mutate(
            strata_level = gsub("and ([^and ]*)$", "", .data$strata_level)
          ) %>%
          dplyr::mutate(strata_level = gsub("[[:space:]]*$", "", .data$strata_level)) %>%
          dplyr::mutate(outcome = dplyr::if_else(.data$outcome == " (s0)", "none",
                                                 dplyr::if_else(.data$outcome == " 1",
                                                                "outcome", "competing_outcome"
                                                 )
          ))

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] %>%
          dplyr::mutate(
            strata = sub(",([^,]*)$", "", .data$strata)
          ) %>%
          dplyr::left_join(
            tidyFit %>%
              dplyr::group_by(.data$strata) %>%
              dplyr::summarise(max_time = max(.data$time, na.rm = TRUE)),
            by = "strata"
          )

        estimates[[i + 1]] <- dplyr::bind_rows(
          dplyr::bind_cols(
            data.frame(
              outcome = 1L,
              time = summ$time,
              strata_level = summ$strata,
              n_event = summ$n.event[, 2],
              n_risk = summ$n.risk[, 1],
              estimate_type = "numeric"
            ),
            as.data.frame(summ$pstate) %>%
              dplyr::rename("estimate" = "V2") %>%
              dplyr::select("estimate"),
            as.data.frame(summ$lower) %>%
              dplyr::rename("estimate_95CI_lower" = "V2") %>%
              dplyr::select("estimate_95CI_lower"),
            as.data.frame(summ$upper) %>%
              dplyr::rename("estimate_95CI_upper" = "V2") %>%
              dplyr::select("estimate_95CI_upper")
          ),
          dplyr::bind_cols(
            data.frame(
              outcome = 2L,
              strata_level = summ$strata,
              time = summ$time,
              n_event = summ$n.event[, 3],
              n_risk = summ$n.risk[, 1],
              estimate_type = "numeric"
            ),
            as.data.frame(summ$pstate) %>%
              dplyr::rename("estimate" = "V3") %>%
              dplyr::select("estimate"),
            as.data.frame(summ$lower) %>%
              dplyr::rename("estimate_95CI_lower" = "V3") %>%
              dplyr::select("estimate_95CI_lower"),
            as.data.frame(summ$upper) %>%
              dplyr::rename("estimate_95CI_upper" = "V3") %>%
              dplyr::select("estimate_95CI_upper")
          )
        ) %>%
          dplyr::mutate(outcome = dplyr::if_else(.data$outcome == 1,
                                                 "outcome",
                                                 "competing_outcome"
          ),
          estimate = .data$estimate,
          estimate_95CI_upper = .data$estimate_95CI_upper,
          estimate_95CI_lower = .data$estimate_95CI_lower
          ) %>%
          dplyr::mutate(analysis_type = "competing_risk")

        estimates[[i + 1]] <- estimates[[i + 1]] %>%
          dplyr::mutate(strata_name = paste(name, collapse = " and ")) %>%
          dplyr::relocate("strata_level", .after = "strata_name") %>%
          dplyr::mutate(strata = .data$strata_level) # to use in below join

        for (j in seq_along(name)) {
          estimates[[i + 1]] <- estimates[[i + 1]] %>%
            dplyr::mutate(strata_level = stringr::str_replace(
              string = .data$strata_level,
              pattern = paste0(name[j], "="), replacement = ""
            )) %>%
            dplyr::mutate(strata_level = stringr::str_replace(
              string = .data$strata_level,
              pattern = ",",
              replacement = " and"
            ))
        }

        # keep estimated probabilities only up to the end of follow up for that group
        maxTimePoints <- fitSummary[[i + 1]] %>%
          dplyr::select("strata_level", "outcome", "max_time")

        estimates[[i + 1]] <- estimates[[i + 1]] %>%
          dplyr::left_join(maxTimePoints,
                           by = c("strata_level", "outcome")
          ) %>%
          dplyr::filter(.data$time <= .data$max_time) %>%
          dplyr::select(!c("max_time", "strata"))

        fitSummary[[i + 1]] <- fitSummary[[i + 1]] %>%
          dplyr::select(!c("max_time", "strata"))
      }}
    cli::cli_progress_done()
  }

  # Output as tibble
  estimates <- dplyr::bind_rows(estimates) %>% dplyr::as_tibble()

  # Get number of events for all eventGaps
  number_events <- estimates %>%
    dplyr::group_by(.data$strata_name, .data$strata_level, .data$outcome) %>%
    dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
    dplyr::filter(.data$time %% eventGap[1] == 0 | .data$time == max(.data$time)) %>%
    dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(eventGap = eventGap[1]) %>%
    dplyr::select(
      "time", "n_risk", "n_events",
      "eventGap", "outcome",
      "strata_name", "strata_level"
    )

  for (t in eventGap[-1]) {
    number_events <- dplyr::union_all(
      number_events,
      estimates %>%
        dplyr::group_by(.data$strata_name, .data$strata_level, .data$outcome) %>%
        dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
        dplyr::filter(.data$time %% t == 0 | .data$time == max(.data$time)) %>%
        dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(eventGap = t) %>%
        dplyr::select(
          "time", "n_risk","n_events", "eventGap", "outcome", "strata_name",
          "strata_level"
        )
    )
  }

  estimates <- estimates %>%
    dplyr::select(!"n_event")

  attr(estimates, "events") <- number_events
  attr(estimates, "summary") <- dplyr::bind_rows(fitSummary)
  row.names(attr(estimates, "summary")) <- NULL
  attr(estimates, "summary") <- dplyr::as_tibble(attr(estimates, "summary"))

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
                             summary = FALSE) {
  outcomeCohortName <- omopgenerics::settings(cdm[[outcomeCohortTable]]) %>%
    dplyr::filter(.data$cohort_definition_id ==
                    .env$outcomeCohortId) %>%
    dplyr::pull("cohort_name")

  x <- x %>%
    dplyr::mutate(
      cdm_name = attr(cdm, "cdm_name"),
      package_name = "CohortSurvival",
      package_version = as.character(utils::packageVersion("CohortSurvival")),
      result_type = "survival_estimate",
      group_name = "cohort",
      group_level =
        omopgenerics::settings(cdm[[targetCohortTable]]) %>%
        dplyr::filter(.data$cohort_definition_id ==
                        .env$targetCohortId) %>%
        dplyr::pull("cohort_name"),
      variable_type = NA,
    )

  if (!is.null(competingOutcomeCohortTable)) {
    competingOutcomeCohortName <- omopgenerics::settings(cdm[[competingOutcomeCohortTable]]) %>%
      dplyr::filter(.data$cohort_definition_id ==
                      .env$competingOutcomeCohortId) %>%
      dplyr::pull("cohort_name")

    if (competingOutcomeCohortName == outcomeCohortName) {
      competingOutcomeCohortName <- paste0(competingOutcomeCohortName, "_competing_outcome")
    }
    x <- x %>%
      dplyr::mutate(outcome = dplyr::if_else(
        .data$outcome == "outcome", outcomeCohortName, competingOutcomeCohortName
      ))
    x <- x %>%
      dplyr::mutate(analysis_type = "competing_risk")
  } else {
    x <- x %>%
      dplyr::mutate(outcome = dplyr::if_else(
        .data$outcome == "outcome", outcomeCohortName, "no_competing_outcome"
      ))
    x <- x %>%
      dplyr::mutate(analysis_type = "single_event")
  }

  return(x)
}

empty_estimates <- function() {
  dplyr::tibble()
}

var_order <- function(estimates) {
  estimates %>%
    dplyr::relocate("cdm_name") %>%
    dplyr::relocate("result_type", .after = "cdm_name") %>%
    dplyr::relocate("package_name", .after = "result_type") %>%
    dplyr::relocate("package_version", .after = "package_name") %>%
    dplyr::relocate("group_name", .after = "package_version") %>%
    dplyr::relocate("group_level", .after = "group_name") %>%
    dplyr::relocate("strata_name", .after = "group_level") %>%
    dplyr::relocate("strata_level", .after = "strata_name") %>%
    dplyr::relocate("variable_name", .after = "strata_level") %>%
    dplyr::relocate("variable_level", .after = "variable_name") %>%
    dplyr::relocate("estimate_name", .after = "variable_level") %>%
    dplyr::relocate("estimate_type", .after = "estimate_name") %>%
    dplyr::relocate("estimate_value", .after = "estimate_type")

}

