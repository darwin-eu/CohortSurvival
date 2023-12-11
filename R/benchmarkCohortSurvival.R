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


#' Estimate performance of estimateSurvival function for benchmarking
#'
#' @param cdm CDM reference
#' @param targetSize number of people in the target cohort table
#' @param outcomeSize number of people in the outcome cohort table
#' @param outcomeDateVariable  Variable containing date of outcome event
#' @param competingOutcomeSize number of people in the competing outcome cohort table
#' @param competingOutcomeDateVariable Variable containing date of
#' competing event
#' @param censorOnCohortExit If TRUE, an individual's follow up will be
#' censored at their cohort exit
#' @param censorOnDate if not NULL, an individual's follow up will be censored
#' at the given date
#' @param followUpDays Number of days to follow up individuals (lower bound 1,
#' upper bound Inf)
#' @param strata strata
#' @param timeGap Days between time points for which to report survival
#' estimates. First day will be day zero with risk estimates provided
#' for times up to the end of follow-up, with a gap in days equivalent
#' to timeGap.
#' @param times vector of time points at which to give survival estimates,
#' if NULL estimates at all times are calculated
#' @param minCellCount The minimum number of events to reported, below which
#' results will be obscured. If 0, all results will be reported.
#' @param returnParticipants Either TRUE or FALSE. If TRUE, references to
#' participants from the analysis will be returned allowing for further
#' analysis.
#'
#' @return tibble with performance of estimateSurvival function information,
#' according to the selected input parameters
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' cdm$condition_occurrence <- cdm$death_cohort %>%
#' dplyr::rename("condition_start_date" = "cohort_start_date",
#'              "condition_end_date" = "cohort_end_date")
#' surv_timings <- benchmarkCohortSurvival(
#' cdm, targetSize = 100, outcomeSize = 20)
#'}
#'
benchmarkCohortSurvival <- function(cdm,
                                    targetSize,
                                    outcomeSize,
                                    outcomeDateVariable = "cohort_start_date",
                                    competingOutcomeSize = NULL,
                                    competingOutcomeDateVariable = "cohort_start_date",
                                    censorOnCohortExit = FALSE,
                                    censorOnDate = NULL,
                                    followUpDays = Inf,
                                    strata = NULL,
                                    timeGap = c(1, 7, 30, 365),
                                    times = NULL,
                                    minCellCount = 5,
                                    returnParticipants = FALSE) {

  # check input
  errorMessage <- checkmate::makeAssertCollection()

  checkCdm(cdm = cdm, tables = c(
    "person", "observation_period"
    ))
  checkmate::assertIntegerish(targetSize,
                              len = 1,
                              lower = 1,
                              add = errorMessage)
  checkmate::assertIntegerish(outcomeSize,
                              len = 1,
                              lower = 1,
                              add = errorMessage)
  checkmate::assertIntegerish(competingOutcomeSize,
                              len = 1,
                              lower = 1,
                              null.ok = TRUE,
                              add = errorMessage)
  checkmate::assertCharacter(outcomeDateVariable,
                             len = 1,
                             add = errorMessage)
  checkmate::assertCharacter(competingOutcomeDateVariable,
                             len = 1,
                             add = errorMessage)
  checkmate::assertLogical(censorOnCohortExit,
                           len = 1,
                           add = errorMessage)
  if(!is.null(censorOnDate)) {
    checkdate <- censorOnDate %>% inherits("Date")
    if(!checkdate) {
      cli::cli_abort("{censorOnDate} is neither NULL nor of type Date")
    }
  }
  if(followUpDays != "Inf") {
    checkmate::assertIntegerish(followUpDays,
                               len = 1,
                               lower = 0,
                               add = errorMessage
                               )
  }
  checkmate::assertIntegerish(timeGap,
                              lower = 1,
                              add = errorMessage
  )
  checkmate::assertIntegerish(times,
                              lower = 0,
                              null.ok = TRUE,
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
                           add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)

  # create cohorts
  timings <- list()
  tictoc::tic()

  targetCohortTable <- "benchmark_target"
  target_cohort <- cdm$person %>%
    dplyr::slice_sample(n = targetSize) %>%
    dplyr::inner_join(cdm$observation_period, by = "person_id") %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::select(
      "subject_id" = "person_id",
      "cohort_definition_id",
      "cohort_start_date" = "observation_period_start_date",
      "cohort_end_date" = "observation_period_end_date"
    ) %>%
    PatientProfiles::addDemographics(cdm) %>%
    dplyr::collect()

  checkStrata(strata, target_cohort)
  targetCohortId <- 1

  if(!is.null(censorOnDate)) {
    target_cohort <- target_cohort %>%
      dplyr::filter(
        .data$cohort_start_date < .env$censorOnDate
      )
  }
  DBI::dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(
    schema = attr(cdm, "write_schema"), table = targetCohortTable
  ), target_cohort, overwrite = TRUE)

  t <- tictoc::toc(quiet = TRUE)
  timings[["target_cohort"]] <- dplyr::tibble(
    task = paste0("generating target cohort size ",targetSize),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )
  tictoc::tic()

  cdm <- CDMConnector::cdm_from_con(attr(cdm, "dbcon"),
                                    attr(cdm, "cdm_schema"),
                                    attr(cdm, "write_schema"),
                                    cohort_tables = c(targetCohortTable),
                                    cdm_name = "benchmark")

  outcomeCohortTable <- "benchmark_outcome"
  min_obs <- cdm[[targetCohortTable]] %>%
    dplyr::select("cohort_start_date") %>%
    dplyr::pull() %>% min()
  max_obs <- cdm[[targetCohortTable]] %>%
    dplyr::select("cohort_end_date") %>%
    dplyr::pull() %>% max()
  outcome_cohort <- dplyr::tibble(
    subject_id = cdm[[targetCohortTable]] %>%
      dplyr::select("subject_id") %>%
      dplyr::pull() %>%
      sample(outcomeSize, replace = TRUE),
    cohort_definition_id = 1,
    cohort_start_date = sample(seq(as.Date(min_obs), as.Date(max_obs), by="day"), outcomeSize),
    cohort_end_date = .data$cohort_start_date
  )

  columnCheck <- outcomeDateVariable %in% colnames(outcome_cohort)
  if(!columnCheck) {
    cli::cli_abort("{outcomeDateVariable} must be `cohort_start_date` or `cohort_end_date`")
  }
  outcomeCohortId <- 1

  DBI::dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(
    schema = attr(cdm, "write_schema"), table = outcomeCohortTable
  ), outcome_cohort, overwrite = TRUE)

  t <- tictoc::toc(quiet = TRUE)
  timings[["outcome_cohort"]] <- dplyr::tibble(
    task = paste0("generating outcome cohort size ",outcomeSize),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )
  tictoc::tic()

  if(!is.null(competingOutcomeSize)) {
    competingOutcomeCohortTable <- "benchmark_competing_outcome"
    competing_outcome_cohort <- dplyr::tibble(
      subject_id = cdm[[targetCohortTable]] %>%
        dplyr::select("subject_id") %>%
        dplyr::pull() %>%
        sample(competingOutcomeSize, replace = TRUE),
      cohort_definition_id = 1,
      cohort_start_date = sample(seq(as.Date(min_obs), as.Date(max_obs), by="day"), competingOutcomeSize),
      cohort_end_date = .data$cohort_start_date
    )
    columnCheck2 <- competingOutcomeDateVariable %in% colnames(competing_outcome_cohort)
    if(!columnCheck2) {
      cli::cli_abort("{competingOutcomeDateVariable} must be `cohort_start_date` or `cohort_end_date`")
    }
    competingOutcomeCohortId <- 1
    DBI::dbWriteTable(attr(cdm, "dbcon"), CDMConnector::inSchema(
      schema = attr(cdm, "write_schema"), table = competingOutcomeCohortTable
    ), competing_outcome_cohort, overwrite = TRUE)

    cdm <- CDMConnector::cdm_from_con(attr(cdm, "dbcon"),
                                      attr(cdm, "cdm_schema"),
                                      attr(cdm, "write_schema"),
                                      cohort_tables = c(targetCohortTable,
                                                        outcomeCohortTable,
                                                        competingOutcomeCohortTable),
                                      cdm_name = "benchmark")
    # this could be a problem if they have other tables loaded (K)


    t <- tictoc::toc(quiet = TRUE)
    timings[["competing_outcome_cohort"]] <- dplyr::tibble(
      task = paste0("generating competing outcome cohort size ",competingOutcomeSize),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
    tictoc::tic()
  } else {
    competingOutcomeCohortTable <- NULL
    competingOutcomeCohortId <- 1
    cdm <- CDMConnector::cdm_from_con(attr(cdm, "dbcon"),
                                      attr(cdm, "cdm_schema"),
                                      attr(cdm, "write_schema"),
                                      cohort_tables = c(targetCohortTable, outcomeCohortTable),
                                      cdm_name = "benchmark")
  }

  workingExposureTable <- cdm[[targetCohortTable]]

  # addCohortSurvival for primary event of interest
  workingExposureTable <- workingExposureTable %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = outcomeCohortTable,
      outcomeCohortId = outcomeCohortId,
      outcomeDateVariable = outcomeDateVariable,
      censorOnCohortExit = censorOnCohortExit,
      censorOnDate = censorOnDate,
      followUpDays = followUpDays
    ) %>%
    dplyr::rename(
      "outcome_time" = "time",
      "outcome_status" = "status"
    )

  t <- tictoc::toc(quiet = TRUE)
  timings[["addCohortSurvival_outcome"]] <- dplyr::tibble(
    task = "addCohortSurvival info added for outcome",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )
  tictoc::tic()

  # competing risk (if there is one)
  if (!is.null(competingOutcomeCohortTable)) {
    workingExposureTable <- workingExposureTable %>%
      addCohortSurvival(
        cdm = cdm,
        outcomeCohortTable = competingOutcomeCohortTable,
        outcomeCohortId = outcomeCohortId,
        outcomeDateVariable = competingOutcomeDateVariable,
        censorOnCohortExit = censorOnCohortExit,
        censorOnDate = censorOnDate,
        followUpDays = followUpDays
      ) %>%
      dplyr::rename(
        "competing_risk_time" = "time",
        "competing_risk_status" = "status"
      )

    t <- tictoc::toc(quiet = TRUE)
    timings[["addCohortSurvival_competing_outcome"]] <- dplyr::tibble(
      task = "addCohortSurvival info added for competing outcome",
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
    tictoc::tic()
  }

  # collect
  survDataDb <- workingExposureTable %>%
    dplyr::filter(!is.na(.data$outcome_time) &&
                    !is.na(.data$outcome_status))

  survData <- survDataDb %>%
    dplyr::collect()

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
  if(!is.null(times)) {
    timepoints <- times
  } else {
    timepoints <- seq(0, max(survData$outcome_time), by = 1)
  }

  # fit survival, with strata
  if (is.null(competingOutcomeCohortTable)) {
    survivalEstimates <- singleEventSurvival(
      survData = survData,
      times = timepoints,
      variables = strata,
      timeGap = timeGap
    )
  } else {
    survivalEstimates <- competingRiskSurvival(
      survData = survData,
      times = timepoints,
      variables = strata,
      timeGap = timeGap
    )
  }

  t <- tictoc::toc(quiet = TRUE)
  timings[["estimateSurvival"]] <- dplyr::tibble(
    task = paste0("estimateSurvival called for specified settings and strata: ",paste0(names(strata), collapse = ", ")),
    time_taken_secs = as.numeric(t$toc - t$tic)
  )
  tictoc::tic()

  if(nrow(survivalEstimates)>0){
    survivalEstimates <- addCohortDetails(
      x = survivalEstimates,
      cdm = cdm,
      targetCohortId = targetCohortId,
      targetCohortTable = targetCohortTable,
      outcomeCohortId = outcomeCohortId,
      outcomeCohortTable = outcomeCohortTable,
      competingOutcomeCohortId = competingOutcomeCohortId,
      competingOutcomeCohortTable = competingOutcomeCohortTable)


    t <- tictoc::toc(quiet = TRUE)
    timings[["counts_obscured"]] <- dplyr::tibble(
      task = paste0("counts obscured < ",minCellCount),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
    tictoc::tic()

    t <- tictoc::toc(quiet = TRUE)
    timings[["counts_obscured"]] <- dplyr::tibble(
      task = paste0("counts obscured < ",minCellCount),
      time_taken_secs = as.numeric(t$toc - t$tic)
    )
    tictoc::tic()

    # add attributes
    if(isTRUE(returnParticipants)){
      participantsRef <-  survDataDb %>%
        dplyr::select("cohort_definition_id",
                      "subject_id",
                      "cohort_start_date",
                      "cohort_end_date") %>%
        CDMConnector::computeQuery(
          name = paste0(attr(cdm, "write_prefix"), "participants"),
          FALSE, attr(cdm, "write_schema"), TRUE
        )

      participantsSetRef <- participantsRef %>%
        dplyr::select("cohort_definition_id") %>%
        dplyr::distinct() %>%
        dplyr::mutate(cohort_name = paste0("survival_participants_",
                                           .data$cohort_definition_id)) %>%
        CDMConnector::computeQuery(
          name = paste0(attr(cdm, "write_prefix"), "participants_set"),
          FALSE, attr(cdm, "write_schema"), TRUE
        )

      participantsCountRef <-  participantsRef %>%
        dplyr::group_by(.data$cohort_definition_id) %>%
        dplyr::summarise(
          number_records = dplyr::n(),
          number_subjects = dplyr::n_distinct(.data$subject_id),
          .groups = "drop"
        ) %>%
        CDMConnector::computeQuery(
          name = paste0(attr(cdm, "write_prefix"), "participants_count"),
          FALSE, attr(cdm, "write_schema"), TRUE
        )

      attr(survivalEstimates, "participants") <- CDMConnector::newGeneratedCohortSet(
        cohortRef = participantsRef,
        cohortSetRef = participantsSetRef,
        cohortCountRef = participantsCountRef
      )

    }

    attr(survivalEstimates, "events") <- addCohortDetails(
      x = attr(survivalEstimates, "events"),
      cdm = cdm,
      targetCohortId = targetCohortId,
      targetCohortTable = targetCohortTable,
      outcomeCohortId = outcomeCohortId,
      outcomeCohortTable = outcomeCohortTable)

  }

  # combine results
  timings <- dplyr::bind_rows(timings) %>%
    dplyr::mutate(time_taken_secs = round(.data$time_taken_secs, 2)) %>%
    dplyr::mutate(time_taken_mins = round(.data$time_taken_secs / 60, 2)) %>%
    dplyr::mutate(time_taken_hours = round(.data$time_taken_mins / 60, 2)) %>%
    dplyr::mutate(dbms = CDMConnector::dbms(cdm)) %>%
    dplyr::mutate(person_n = cdm$person %>%
                    dplyr::count() %>%
                    dplyr::pull()) %>%
    dplyr::mutate(db_min_observation_start = cdm$observation_period %>%
                    dplyr::summarise(
                      db_min_obs_start =
                        min(.data$observation_period_start_date,
                            na.rm = TRUE
                        )
                    ) %>%
                    dplyr::pull()) %>%
    dplyr::mutate(max_observation_end = cdm$observation_period %>%
                    dplyr::summarise(
                      max_observation_end =
                        max(.data$observation_period_end_date,
                            na.rm = TRUE
                        )
                    ) %>%
                    dplyr::pull())

  if (isFALSE(returnParticipants)) {
    timings <- timings %>%
      dplyr::mutate(with_participants = "No")
  } else {
    timings <- timings %>%
      dplyr::mutate(with_participants = "Yes")
  }

  return(timings)
}

insertTable <- function(x,
                        cdm,
                        name,
                        overwrite = TRUE) {
  con <- attr(cdm, "dbcon")
  writeSchema <- attr(cdm, "write_schema")
  checkTableExist <- name %in% CDMConnector::listTables(con, writeSchema)
  if (checkTableExist) {
    if (overwrite) {
      DBI::dbRemoveTable(con, CDMConnector::inSchema(writeSchema, name))
    } else {
      stop(paste0("'", name, "' table already exists."))
    }
  }
  DBI::dbCreateTable(con, CDMConnector::inSchema(writeSchema, name), x)
  DBI::dbAppendTable(con, CDMConnector::inSchema(writeSchema, name), x)
  if (methods::is(con, "duckdb_connection")) {
    ref <- dplyr::tbl(con, paste(c(writeSchema, name), collapse = "."))
  } else if (length(writeSchema) == 2) {
    ref <- dplyr::tbl(con,
                      dbplyr::in_catalog(writeSchema[[1]], writeSchema[[2]], name))
  } else if (length(writeSchema) == 1) {
    ref <- dplyr::tbl(con, dbplyr::in_schema(writeSchema, name))
  } else {
    ref <- dplyr::tbl(con, name)
  }
  return(ref)
}
