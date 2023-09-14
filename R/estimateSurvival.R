#' Estimate survival for a given event of interest using cohorts in the OMOP Common Data Model
#'
#' @param cdm CDM reference
#' @param targetCohortTable targetCohortTable
#' @param targetCohortId targetCohortId
#' @param outcomeCohortTable The outcome cohort table of interest.
#' @param outcomeCohortId ID of event cohorts to include. Only one outcome
#' (and so one ID) can be considered.
#' @param outcomeDateVariable  Variable containing date of outcome event
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
#' @return tibble with survival information for desired cohort, including:
#' time, people at risk, survival probability, cumulative incidence,
#' 95 CIs, strata and outcome. A tibble with the number of events is
#' outputted as an attribute of the output
#' @export
#'
#' @examples
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(cdm,
#'                        targetCohortTable = "mgus_diagnosis",
#'                        targetCohortId = 1,
#'                        outcomeCohortTable = "death_cohort",
#'                        outcomeCohortId = 1,
#'                        timeGap = 7
#' )
#'
estimateSingleEventSurvival <- function(cdm,
                             targetCohortTable,
                             targetCohortId = 1,
                             outcomeCohortTable,
                             outcomeCohortId = 1,
                             outcomeDateVariable = "cohort_start_date",
                             censorOnCohortExit = FALSE,
                             censorOnDate = NULL,
                             followUpDays = Inf,
                             strata = NULL,
                             timeGap = c(1, 7, 30, 365),
                             times = NULL,
                             minCellCount = 5,
                             returnParticipants = FALSE) {

  estimateSurvival(cdm = cdm,
    targetCohortTable = targetCohortTable,
    targetCohortId = targetCohortId,
    outcomeCohortTable = outcomeCohortTable,
    outcomeCohortId = outcomeCohortId,
    outcomeDateVariable = outcomeDateVariable,
    competingOutcomeCohortTable = NULL,
    competingOutcomeCohortId = 1,
    competingOutcomeDateVariable = "cohort_start_date",
    censorOnCohortExit = censorOnCohortExit,
    censorOnDate = censorOnDate,
    followUpDays = followUpDays,
    strata = strata,
    timeGap = timeGap,
    times = times,
    minCellCount = minCellCount,
    returnParticipants = returnParticipants
  )

}


estimateSurvival <- function(cdm,
                             targetCohortTable,
                             targetCohortId = 1,
                             outcomeCohortTable,
                             outcomeCohortId = 1,
                             outcomeDateVariable = "cohort_start_date",
                             competingOutcomeCohortTable = NULL,
                             competingOutcomeCohortId = 1,
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

  checkmate::assertCharacter(targetCohortTable,
                             len = 1,
                             add = errorMessage)
  checkmate::assertCharacter(outcomeCohortTable,
                             len = 1,
                             add = errorMessage)
  checkmate::assertCharacter(competingOutcomeCohortTable,
                             len = 1,
                             null.ok = TRUE,
                             add = errorMessage)
  checkCdm(cdm, tables = c(
    "person", "observation_period",
    targetCohortTable,
    outcomeCohortTable
  ))

  checkIsCohort_exp(cdm[[targetCohortTable]])
  checkmate::assertIntegerish(targetCohortId,
                              len = 1,
                              lower = 1,
                              add = errorMessage)
  checkStrata(strata, cdm[[targetCohortTable]])
  checkmate::assertIntegerish(outcomeCohortId,
                              len = 1,
                              lower = 1,
                              add = errorMessage)
  checkmate::assertIntegerish(competingOutcomeCohortId,
                              len = 1,
                              lower = 1,
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
    surv <- singleEventSurvival(
      survData = survData,
      times = timepoints,
      variables = strata,
      timeGap = timeGap
    )
  } else {
    surv <- competingRiskSurvival(
      survData = survData,
      times = timepoints,
      variables = strata,
      timeGap = timeGap
    )
  }

  if(nrow(surv)>0){
  survivalEstimates <-  addCohortDetails(
    x = surv,
    cdm = cdm,
    targetCohortId = targetCohortId,
    targetCohortTable = targetCohortTable,
    outcomeCohortId = outcomeCohortId,
    outcomeCohortTable = outcomeCohortTable,
    competingOutcomeCohortId = competingOutcomeCohortId,
    competingOutcomeCohortTable = competingOutcomeCohortTable)

    survivalEstimates <- survivalEstimates %>%
      tidyr::pivot_longer(cols = c("outcome_cohort_name",
                                   "competing_outcome_cohort_name"),
                          names_to = "variable",
                          values_to = "variable_level") %>%
      dplyr::filter(.data$variable_level != "No competing outcome") %>%
      dplyr::mutate(variable = "Outcome")
    # %>%
    #   dplyr::select(!"outcome")

    survivalEstimates <- survivalEstimates %>%
      dplyr::select(!"variable_type") %>%
      tidyr::pivot_longer(
        cols = c(
          "n_risk",
          "estimate",
          "estimate_95CI_lower",
          "estimate_95CI_upper"
        ),
        names_to = "variable_type",
        values_to = "estimate"
      )

    survivalEstimates <- var_order(survivalEstimates) %>%
      dplyr::distinct()


  # add attributes
  if(isTRUE(returnParticipants)){
    participantsRef <-  survDataDb %>%
      dplyr::select("cohort_definition_id",
                    "subject_id",
                    "cohort_start_date",
                    "cohort_end_date") %>%
      CDMConnector::computeQuery()

    participantsSetRef <- participantsRef %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::mutate(cohort_name = paste0("survival_participants_",
                                         .data$cohort_definition_id)) %>%
      dplyr::collect()

    participantsCountRef <-  participantsRef %>%
      dplyr::group_by(.data$cohort_definition_id) %>%
      dplyr::summarise(
        number_records = dplyr::n(),
        number_subjects = dplyr::n_distinct(.data$subject_id),
        .groups = "drop"
      ) %>%
      dplyr::collect()

    attr(survivalEstimates, "participants") <- CDMConnector::newGeneratedCohortSet(
      cohortRef = participantsRef ,
      cohortSetRef = participantsSetRef,
      cohortCountRef = participantsCountRef
    )

  }

  attr(survivalEstimates, "events") <- addCohortDetails(
                   x = attr(surv, "events"),
                   cdm = cdm,
                   targetCohortId = targetCohortId,
                   targetCohortTable = targetCohortTable,
                   outcomeCohortId = outcomeCohortId,
                   outcomeCohortTable = outcomeCohortTable,
                   competingOutcomeCohortTable = competingOutcomeCohortTable,
                   competingOutcomeCohortId = competingOutcomeCohortId
                   ) %>%
    dplyr::rename("estimate" = "n_events") %>%
    dplyr::mutate(
      variable = "Outcome",
      variable_level = paste0("timeGap ", timeGap),
      estimate_type = "Survival events",
      variable_type = "n_events"
    ) %>%
    dplyr::select(-c("outcome_cohort_name", "competing_outcome_cohort_name", "timeGap")) %>%
    var_order() %>%
    dplyr::relocate("outcome",
                    .after = "variable_type") %>%
    dplyr::relocate("time",
                    .after = "outcome") %>%
    dplyr::relocate("analysis_type",
                    .after = "time") %>%
    dplyr::relocate("estimate",
                    .after = "analysis_type")

  survivalEstimates <- dplyr::union_all(
    survivalEstimates,
    attr(survivalEstimates, "events")
  )

  attr(survivalEstimates, "events") <- NULL

  attr(survivalEstimates, "attrition") <- attrition

  if (is.null(competingOutcomeCohortTable)) {
  attr(survivalEstimates, "summary") <- addCohortDetails(
    x = attr(surv, "summary"),
    cdm = cdm,
    targetCohortId = targetCohortId,
    targetCohortTable = targetCohortTable,
    outcomeCohortId = outcomeCohortId,
    outcomeCohortTable = outcomeCohortTable) %>%
    dplyr::mutate(analysis_type = "Single event")

  } else {
    attr(survivalEstimates, "summary") <- addCohortDetails(
      x = attr(surv, "summary"),
      cdm = cdm,
      targetCohortId = targetCohortId,
      targetCohortTable = targetCohortTable,
      outcomeCohortId = outcomeCohortId,
      outcomeCohortTable = outcomeCohortTable,
      competingOutcomeCohortTable = competingOutcomeCohortTable,
      competingOutcomeCohortId = competingOutcomeCohortId) %>%
      dplyr::mutate(analysis_type = "Competing risk")
  }

  attr(survivalEstimates, "summary") <- attr(survivalEstimates, "summary") %>%
    dplyr::mutate(result_type = "Survival estimate",
                  variable = "Outcome",
                  variable_level = CDMConnector::cohortSet(cdm[[targetCohortTable]]) %>%
                    dplyr::filter(.data$cohort_definition_id ==
                                    .env$targetCohortId) %>%
                    dplyr::pull("cohort_name"),
                  estimate_type = "Survival summary") %>%
    dplyr::select(-c("outcome_cohort_name", "competing_outcome_cohort_name", "variable_type")) %>%
    tidyr::pivot_longer(cols = -c("cdm_name",
                                  "result_type",
                                  "group_name",
                                  "group_level",
                                  "strata_name",
                                  "strata_level",
                                  "variable",
                                  "variable_level",
                                  "estimate_type",
                                  "outcome",
                                  "analysis_type"),
                        names_to = "variable_type",
                        values_to = "estimate") %>%
    var_order() %>%
    dplyr::mutate(time = NA) %>%
    dplyr::relocate("outcome",
                    .after = "variable_type") %>%
    dplyr::relocate("time",
                    .after = "outcome") %>%
    dplyr::relocate("analysis_type",
                    .after = "time") %>%
    dplyr::relocate("estimate",
                    .after = "analysis_type")

  survivalEstimates <- dplyr::union_all(
    survivalEstimates,
    attr(survivalEstimates, "summary")
  )

  attr(survivalEstimates, "summary") <- NULL

  }

  # obscure counts below minCellCount
  survivalEstimates <- suppressSurvivalCounts(survivalEstimates, minCellCount)

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
      .data[[time2]] < .data[[time1]],
      2 * .data[[status2]], .data[[status1]]
    )))

  return(data)
}

singleEventSurvival <- function(survData, times, variables, timeGap) {
  estimates <- list()
  fitSummary <- list()


  var_columns <- unlist(variables) %>% unique()

  cli::cli_progress_message("Getting overall estimates")
    fit <- survival::survfit(survival::Surv(outcome_time, outcome_status) ~ 1,
                             data = survData
    )

  fitSummary[[1]] <- as.data.frame(t(summary(fit)$table)) %>%
      dplyr::rename("n_max" = "n.max",
                    "n_start" = "n.start",
                    "restricted_mean"= "rmean",
                    "restricted_mean_std_error"= "se(rmean)",
                    "median_survival" = "median",
                    "median_survival_95CI_lower" = "0.95LCL",
                    "median_survival_95CI_higher" = "0.95UCL") %>%
      dplyr::mutate(analysis_type = "single event") %>%
      dplyr::mutate(strata_name = "Overall",
                    strata_level = "Overall",
                    outcome = "outcome")

  summ <- summary(fit, times = times, extend = TRUE)
  estimates[[1]] <-  dplyr::bind_rows(
    dplyr::tibble(
      outcome = "outcome",
      time = summ$time,
      n_event = summ$n.event,
      n_risk = summ$n.risk,
      estimate_type = "Survival probability",
      estimate = summ$surv,
      estimate_95CI_lower = summ$lower,
      estimate_95CI_upper = summ$upper
    ),
    dplyr::tibble(
      outcome = "outcome",
      time = summ$time,
      n_event = summ$n.event,
      n_risk = summ$n.risk,
      estimate_type = "Cumulative failure probability",
      estimate = 1 - summ$surv,
      estimate_95CI_lower = 1 - summ$upper,
      estimate_95CI_upper = 1 - summ$lower
    )) %>%
    dplyr::mutate(analysis_type = "Single event",
                  strata_name = "Overall",
                  strata_level = "Overall")

  # Add strata estimates if required
  if(!is.null(variables)) {
    cli::cli_progress_bar(
      total = length(variables),
      format = " -- Getting estimates for {cli::pb_bar} {cli::pb_current} of {cli::pb_total} strata"
    )
    for(i in seq_along(variables)) {
      cli::cli_progress_update()
      # Get formula for the model
      name <- variables[[i]]
      expr <- stats::as.formula(paste(c("survival::Surv(outcome_time, outcome_status) ~ 1",
                    name), collapse = " + "))
      fit <- survival::survfit(expr, data = survData)

      fitSummary[[i+1]] <- as.data.frame(summary(fit)$table) %>%
        dplyr::rename("n_max" = "n.max",
                      "n_start" = "n.start",
                      "restricted_mean"= "rmean",
                      "restricted_mean_std_error"= "se(rmean)",
                      "median_survival" = "median",
                      "median_survival_95CI_lower" = "0.95LCL",
                      "median_survival_95CI_higher" = "0.95UCL") %>%
        dplyr::mutate(analysis_type = "single event",
                      outcome = "outcome")

      summ <- summary(fit, times = times, extend = TRUE)
      estimates[[i+1]] <- dplyr::bind_rows(
        dplyr::tibble(
          outcome = "outcome",
          time = summ$time,
          n_event = summ$n.event,
          n_risk = summ$n.risk,
          estimate_type = "Survival probability",
          estimate = summ$surv,
          estimate_95CI_lower = summ$lower,
          estimate_95CI_upper = summ$upper
        ),
        dplyr::tibble(
          outcome = "outcome",
          time = summ$time,
          n_event = summ$n.event,
          n_risk = summ$n.risk,
          estimate_type = "Cumulative failure probability",
          estimate = 1 - summ$surv,
          estimate_95CI_lower = 1 - summ$upper,
          estimate_95CI_upper = 1 - summ$lower
        )) %>%
        dplyr::mutate(analysis_type = "Single event")

      # Add strata variable columns in a good format
      for(j in seq_along(name)) {
        name_w = name
        estimates[[i+1]] <- estimates[[i+1]] %>%
          dplyr::mutate(
            strata_name = paste(name_w, collapse = " and "),
            strata_level = rep(gsub(", "," and ",gsub(paste(paste0(name_w,"="),
                                                     collapse="|"),"",
                                               summ$strata)),2)
            )

        fitSummary[[i+1]] <- fitSummary[[i+1]] %>%
          dplyr::mutate(
            strata_name = paste(name_w, collapse = " and "),
            strata_level = gsub(", "," and ",gsub(paste(paste0(name_w,"="),
                                                     collapse="|"),"",
                                               row.names(fitSummary[[i+1]])))
          )

      }
    }
    cli::cli_progress_done()
  }

  # Output as tibble
  estimates <- dplyr::bind_rows(estimates)



  # Get number of events for all timeGaps
  number_events <- estimates %>%
    dplyr::filter(.data$estimate_type == "Survival probability") %>%
    dplyr::group_by(.data$strata_name, .data$strata_level) %>%
    dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
    dplyr::filter(.data$time %% timeGap[1] == 0 | .data$time == max(.data$time)) %>%
    dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeGap = timeGap[1],
                  outcome = "outcome") %>%
    dplyr::select("time", "n_events", "timeGap", "outcome",
                  "strata_name", "strata_level")

  for(t in timeGap[-1]) {
    number_events <- dplyr::union_all(
      number_events,
      estimates %>%
        dplyr::filter(.data$estimate_type == "Survival probability") %>%
        dplyr::group_by(.data$strata_name, .data$strata_level) %>%
        dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
        dplyr::filter(.data$time %% t == 0 | .data$time == max(.data$time)) %>%
        dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(timeGap = t) %>%
        dplyr::select("time", "n_events", "timeGap", "outcome",
                      "strata_name", "strata_level")
    )
  }

  estimates <- estimates %>%
    dplyr::select(- .data$n_event)

  attr(estimates, "events") <- number_events
  attr(estimates, "summary") <- dplyr::bind_rows(fitSummary)
  row.names(attr(estimates, "summary")) <- NULL
  attr(estimates, "summary") <- dplyr::as_tibble(attr(estimates, "summary"))


  return(estimates)
}

competingRiskSurvival <- function(survData, times, variables, timeGap) {

  if(!length(unique(survData$outcome_or_competing_status)) == 3){

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
  fit <- survival::survfit(formula = survival::Surv(outcome_or_competing_time,
                         outcome_or_competing_status) ~ 1,
          data = survData)
  summ <- summary(fit, times = times, extend = TRUE)

  fitSummary[[1]] <- as.data.frame(summary(fit)$table) %>%
    dplyr::rename("n_start" = "n",
                  "restricted_mean"= "rmean",
                  "n_events"= "nevent") %>%
    dplyr::mutate(analysis_type = "competing risk") %>%
    dplyr::mutate(strata_name = "Overall",
                  strata_level = "Overall") %>%
    tibble::rownames_to_column(var = "outcome") %>%
    dplyr::mutate(outcome = dplyr::if_else(.data$outcome == "(s0)", "none",
                                   dplyr::if_else(.data$outcome == "1",
                                                  "outcome", "competing outcome")))

  estimates[[1]] <- dplyr::bind_rows(
    dplyr::bind_cols(
   data.frame(outcome = 1L,
              time = summ$time,
              n_event = summ$n.event[,2],
              n_risk = summ$n.risk[,1],
              estimate_type = "Cumulative failure probability"),
   as.data.frame(summ$pstate) %>%
    dplyr::rename("estimate" = "V2") %>%
    dplyr::select("estimate"),
  as.data.frame(summ$lower) %>%
    dplyr::rename("estimate_95CI_lower" = "V2") %>%
    dplyr::select("estimate_95CI_lower"),
  as.data.frame(summ$upper) %>%
    dplyr::rename("estimate_95CI_upper" = "V2") %>%
    dplyr::select("estimate_95CI_upper")),
 dplyr::bind_cols(
   data.frame(outcome = 2L,
              time = summ$time,
              n_event = summ$n.event[,3],
              n_risk = summ$n.risk[,1],
              estimate_type = "Cumulative failure probability"),
   as.data.frame(summ$pstate) %>%
     dplyr::rename("estimate" = "V3") %>%
     dplyr::select("estimate"),
   as.data.frame(summ$lower) %>%
     dplyr::rename("estimate_95CI_lower" = "V3") %>%
     dplyr::select("estimate_95CI_lower"),
   as.data.frame(summ$upper) %>%
     dplyr::rename("estimate_95CI_upper" = "V3") %>%
     dplyr::select("estimate_95CI_upper"))) %>%
   dplyr::mutate(outcome = dplyr::if_else(.data$outcome == 1,
                                          "outcome",
                                          "competing outcome"
   )) %>%
   dplyr::mutate(analysis_type = "Competing risk") %>%
   dplyr::mutate(strata_name = "Overall",
                 strata_level = "Overall")

  # Add strata estimates if required
  if(!is.null(variables)) {
    cli::cli_progress_bar(
      total = length(variables),
      format = " -- Getting estimates for {cli::pb_bar} {cli::pb_current} of {cli::pb_total} strata"
    )
    for(i in seq_along(variables)) {
     cli::cli_progress_update()
      # Get formula for the model
      name <- variables[[i]]
      expr <- stats::as.formula(paste(c("survival::Surv(outcome_or_competing_time, outcome_or_competing_status) ~ 1",
                      name), collapse = " + "))
      fit <- survival::survfit(formula = expr,
                        data = survData %>%
                          dplyr::filter(dplyr::if_any(.env$name, ~ !is.na(.))))
      summ <- summary(fit, times = times, extend = TRUE)

      fitSummary[[i+1]] <- as.data.frame(summary(fit)$table) %>%
        dplyr::rename("n_start" = "n",
                      "restricted_mean"= "rmean",
                      "n_events"= "nevent") %>%
        dplyr::mutate(analysis_type = "competing risk") %>%
        dplyr::mutate(strata_name = "Overall",
                      strata_level = "Overall") %>%
        tibble::rownames_to_column(var = "outcome") %>%
        dplyr::mutate(outcome = dplyr::if_else(.data$outcome == "(s0)", "none",
                                               dplyr::if_else(.data$outcome == "1",
                                                              "outcome", "competing outcome")))

      estimates[[i+1]] <- dplyr::bind_rows(
        dplyr::bind_cols(
          data.frame(outcome = 1L,
                     time = summ$time,
                     strata_level = summ$strata,
                     n_event = summ$n.event[,2],
                     n_risk = summ$n.risk[,1],
                     estimate_type = "Cumulative failure probability"),
          as.data.frame(summ$pstate) %>%
            dplyr::rename("estimate" = "V2") %>%
            dplyr::select("estimate"),
          as.data.frame(summ$lower) %>%
            dplyr::rename("estimate_95CI_lower" = "V2") %>%
            dplyr::select("estimate_95CI_lower"),
          as.data.frame(summ$upper) %>%
            dplyr::rename("estimate_95CI_upper" = "V2") %>%
            dplyr::select("estimate_95CI_upper")),
        dplyr::bind_cols(
          data.frame(outcome = 2L,
                     strata_level = summ$strata,
                     time = summ$time,
                     n_event = summ$n.event[,3],
                     n_risk = summ$n.risk[,1],
                     estimate_type = "Cumulative failure probability"),
          as.data.frame(summ$pstate) %>%
            dplyr::rename("estimate" = "V3") %>%
            dplyr::select("estimate"),
          as.data.frame(summ$lower) %>%
            dplyr::rename("estimate_95CI_lower" = "V3") %>%
            dplyr::select("estimate_95CI_lower"),
          as.data.frame(summ$upper) %>%
            dplyr::rename("estimate_95CI_upper" = "V3") %>%
            dplyr::select("estimate_95CI_upper"))) %>%
        dplyr::mutate(outcome = dplyr::if_else(.data$outcome == 1,
                                               "outcome",
                                               "competing outcome"
        )) %>%
        dplyr::mutate(analysis_type = "Competing risk") %>%
        dplyr::mutate(outcome = dplyr::if_else(.data$outcome == 1,
                                               "outcome",
                                               "competing outcome"
        ))

      estimates[[i+1]] <-  estimates[[i+1]] %>%
        dplyr::mutate(strata_name = paste(name, collapse = " and ")) %>%
        dplyr::relocate("strata_level",.after = "strata_name")

      for(j in seq_along(name)) {
        estimates[[i+1]] <- estimates[[i+1]] %>%
          dplyr::mutate(strata_level= stringr::str_replace(string = .data$strata_level,
            pattern = paste0(name[j], "="), replacement = "")) %>%
          dplyr::mutate(strata_level= stringr::str_replace(string = .data$strata_level,
                                                           pattern = ",",
                                                           replacement = " and"))
        fitSummary[[i+1]] <- fitSummary[[i+1]] %>%
          dplyr::mutate(
            strata_name = paste(name, collapse = " and "),
            strata_level = gsub(", "," and ",gsub(paste(paste0(name,"="),
                                                        collapse="|"),"",
                                                  row.names(fitSummary[[i+1]])))
          )
      }
    }
    cli::cli_progress_done()
  }

  # Output as tibble
  estimates <- dplyr::bind_rows(estimates) %>% dplyr::as_tibble()

  # Get number of events for all timeGaps
  number_events <- estimates %>%
    dplyr::group_by(.data$strata_name, .data$strata_level, .data$outcome) %>%
    dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
    dplyr::filter(.data$time %% timeGap[1] == 0 | .data$time == max(.data$time)) %>%
    dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timeGap = timeGap[1]) %>%
    dplyr::select(.data$time, .data$n_events, .data$timeGap, .data$outcome, .data$strata_name, .data$strata_level)

  for(t in timeGap[-1]) {
    number_events <- dplyr::union_all(
      number_events,
      estimates %>%
        dplyr::group_by(.data$strata_name, .data$strata_level, .data$outcome) %>%
        dplyr::mutate(n_events = cumsum(.data$n_event)) %>%
        dplyr::filter(.data$time %% t == 0 | .data$time == max(.data$time)) %>%
        dplyr::mutate(n_events = c(.data$n_events[1], diff(.data$n_events))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(timeGap = t) %>%
        dplyr::select(.data$time, .data$n_events, .data$timeGap, .data$outcome, .data$strata_name, .data$strata_level)
    )
  }

  estimates <- estimates %>%
    dplyr::select(- .data$n_event)

  attr(estimates, "events") <- number_events
  attr(estimates, "summary") <- dplyr::bind_rows(fitSummary)
  row.names(attr(estimates, "summary")) <- NULL
  attr(estimates, "summary") <- dplyr::as_tibble(attr(estimates, "summary"))
  # add summary attribute

  return(estimates)

}

addCohortDetails <- function(x,
                             cdm,
                             targetCohortId,
                             targetCohortTable,
                             outcomeCohortId,
                             outcomeCohortTable,
                             competingOutcomeCohortId,
                             competingOutcomeCohortTable = NULL){

 x <- x %>%
    dplyr::mutate(cdm_name = attr(cdm, "cdm_name"),
                  result_type = "Survival estimate",
                  group_name = "Cohort",
                  group_level =
                    CDMConnector::cohortSet(cdm[[targetCohortTable]]) %>%
                    dplyr::filter(.data$cohort_definition_id ==
                                    .env$targetCohortId) %>%
                    dplyr::pull("cohort_name"),
                  outcome_cohort_name =
                    CDMConnector::cohortSet(cdm[[outcomeCohortTable]]) %>%
                    dplyr::filter(.data$cohort_definition_id ==
                                    .env$outcomeCohortId) %>%
                    dplyr::pull("cohort_name"),
                  variable_type = NA)

 if(!is.null(competingOutcomeCohortTable)){
   x <- x %>%
     dplyr::mutate(competing_outcome_cohort_name =
                     CDMConnector::cohortSet(cdm[[competingOutcomeCohortTable]]) %>%
                     dplyr::filter(.data$cohort_definition_id ==
                                     .env$competingOutcomeCohortId) %>%
                     dplyr::pull("cohort_name"),
                   analysis_type = "Competing risk")
 } else {
   x <- x %>%
     dplyr::mutate(competing_outcome_cohort_name = "No competing outcome",
                   analysis_type = "Single event")
 }

 x <- x %>%
   dplyr::relocate("competing_outcome_cohort_name",
                   .after = "outcome_cohort_name")


 return(x)

}

empty_estimates <- function(){
dplyr::tibble()

}

var_order <- function(estimates){
  estimates %>%
    dplyr::relocate("cdm_name") %>%
    dplyr::relocate("result_type", .after = "cdm_name") %>%
    dplyr::relocate("group_name", .after = "result_type") %>%
    dplyr::relocate("group_level", .after = "group_name") %>%
    dplyr::relocate("strata_name", .after = "group_level") %>%
    dplyr::relocate("strata_level", .after = "strata_name") %>%
    dplyr::relocate("variable", .after = "strata_level") %>%
    dplyr::relocate("variable_level", .after = "variable") %>%
    dplyr::relocate("variable_type", .after = "variable_level") %>%
    dplyr::relocate("estimate_type", .after = "variable_level")

}

