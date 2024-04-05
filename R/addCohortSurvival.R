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

#' Add survival information to a cohort table
#' @param x cohort table to add survival information
#' @param cdm CDM reference
#' @param outcomeCohortTable The outcome cohort table of interest.
#' @param outcomeCohortId ID of event cohorts to include. Only one outcome
#' (and so one ID) can be considered.
#' @param outcomeDateVariable Variable containing date of outcome event
#' @param outcomeWashout Washout time in days for the outcome
#' @param censorOnCohortExit If TRUE, an individual's follow up will be
#' censored at their cohort exit
#' @param censorOnDate if not NULL, an individual's follow up will be censored
#' at the given date
#' @param followUpDays Number of days to follow up individuals (lower bound 1,
#' upper bound Inf)
#'
#' @return Two additional columns will be added to x. The "time" column will
#' contain number of days to censoring. The "status" column will indicate
#' whether the patient had the event (value: 1), or did not have the event
#' (value: 0)
#' @export
#'
#' @examples
#' \donttest{
#'
#' cdm <- mockMGUS2cdm()
#' cdm$mgus_diagnosis <- cdm$mgus_diagnosis %>%
#'   addCohortSurvival(
#'     cdm = cdm,
#'     outcomeCohortTable = "death_cohort",
#'     outcomeCohortId = 1
#'   )
#'   }
#'
addCohortSurvival <- function(x,
                              cdm,
                              outcomeCohortTable,
                              outcomeCohortId = 1,
                              outcomeDateVariable = "cohort_start_date",
                              outcomeWashout = Inf,
                              censorOnCohortExit = FALSE,
                              censorOnDate = NULL,
                              followUpDays = Inf) {

  validateExtractSurvivalInputs(
    cdm = cdm,
    cohortTable = x,
    outcomeCohortTable = outcomeCohortTable,
    outcomeCohortId = outcomeCohortId,
    outcomeWashout = outcomeWashout,
    censorOnCohortExit = censorOnCohortExit,
    censorOnDate = censorOnDate,
    followUpDays = followUpDays
  )

  # drop columns if they already exist
  x <- x %>%
    dplyr::select(!dplyr::any_of(c("days_to_exit",
                                 "time",
                                 "status")))

  # get time to end of observation period
  x <- x %>%
    PatientProfiles::addFutureObservation(
      indexDate = "cohort_start_date",
      futureObservationName = "days_to_exit"
    ) %>% dplyr::compute()

  # get any events before or after index date
  x <- x %>%
    PatientProfiles::addCohortIntersectFlag(
      indexDate = "cohort_start_date",
      targetCohortTable = outcomeCohortTable,
      targetCohortId = outcomeCohortId,
      window = c(-outcomeWashout,-1),
      nameStyle = "event_in_washout"
    ) %>%
    PatientProfiles::addCohortIntersectDays(
      indexDate = "cohort_start_date",
      targetCohortTable = outcomeCohortTable,
      targetCohortId = outcomeCohortId,
      targetDate = outcomeDateVariable,
      window = c(0, Inf),
      nameStyle = "days_to_event"
    ) %>% dplyr::compute()

  # whatever comes first

  # censor at first of
  # 1) outcome,
  # 2) end of observation period
  # 3) cohort exit (if censorOnCohortExit is TRUE)
  # 4) followUpDays (if followUpDays is not Inf)

  if (isTRUE(censorOnCohortExit)) {
    x <- x %>%
      dplyr::mutate(days_end_cohort = !!CDMConnector::datediff(
        "cohort_start_date", "cohort_end_date")) %>%
      dplyr::mutate(days_to_event = dplyr::if_else(
        .data$days_to_event <= .data$days_end_cohort,
        .data$days_to_event, as.numeric(NA)
      )) %>%
      dplyr::mutate(days_to_exit = dplyr::if_else(
        .data$days_to_exit < .data$days_end_cohort,
        .data$days_to_exit, .data$days_end_cohort
      )) %>%
      dplyr::select(!"days_end_cohort") %>%
      dplyr::compute()
  }

  if (!is.null(censorOnDate)) {
    x <- x %>%
      dplyr::mutate(censor_date = .env$censorOnDate) %>%
      dplyr::mutate(days_to_censor = !!CDMConnector::datediff(
        "cohort_start_date", "censor_date"
      )) %>%
      dplyr::mutate(days_to_event = dplyr::if_else(
        .data$days_to_event >= .data$days_to_censor,
        as.numeric(NA), .data$days_to_event
      )) %>%
      dplyr::mutate(days_to_exit = dplyr::if_else(
        .data$days_to_exit < .data$days_to_censor,
        .data$days_to_exit, .data$days_to_censor
      )) %>%
      dplyr::select(!c("days_to_censor", "censor_date")) %>%
      dplyr::compute()
  }

  if (followUpDays != Inf) {
    x <- x %>%
      dplyr::mutate(days_to_event = dplyr::if_else(
        .data$days_to_event <= .env$followUpDays,
        .data$days_to_event, as.numeric(NA)
      )) %>%
      dplyr::mutate(days_to_exit = dplyr::if_else(
        .data$days_to_exit < .env$followUpDays,
        .data$days_to_exit, .env$followUpDays
      )) %>%
      dplyr::compute()
  }

  # now just using days_to_event and days_to_exit
  # add status variable (1 if event, 0 if not)
  # add time variable (days to event for those with event,
  # days to exit if no event)
  x <- x %>%
    dplyr::mutate(status = dplyr::if_else(
      !is.na(.data$days_to_event), 1, 0
    )) %>%
    dplyr::mutate(time = dplyr::if_else(.data$status == 1,
      .data$days_to_event, .data$days_to_exit
    ))

  # for anyone with an outcome in the washout
  # we keep them, but their time and event will be set to NA
  # (ie they won't contribute to any analysis)
  x <- x %>%
    dplyr::mutate(
      status = dplyr::if_else(!is.na(.data$event_in_washout) &&
                               .data$event_in_washout == 1, NA,
        .data$status
      ),
      time = dplyr::if_else(!is.na(.data$event_in_washout) &&
                               .data$event_in_washout == 1, NA,
        .data$time
      )
    )
  # likewise if we censor on a date prior to their cohort start date
  if(!is.null(censorOnDate)) {
    x <- x %>%
      dplyr::mutate(
        status = dplyr::if_else(.data$cohort_start_date > .env$censorOnDate, NA,
                                .data$status
        ),
        time = dplyr::if_else(.data$cohort_start_date > .env$censorOnDate, NA,
                              .data$time
        )
      )
  }

  x <- x %>%
    dplyr::select(!c("event_in_washout", "days_to_event"))

  return(x)
}


validateExtractSurvivalInputs <- function(cdm,
                                          cohortTable,
                                          outcomeCohortTable,
                                          outcomeCohortId,
                                          outcomeWashout,
                                          censorOnCohortExit,
                                          censorOnDate,
                                          followUpDays) {
  checkCdm(cdm, tables = c(
    "person", "observation_period",
    outcomeCohortTable
  ))

  checkIsCohort_exp(cohortTable)
  checkExposureCohortId(cohortTable)

  checkIsCohort(cdm[[outcomeCohortTable]])

  checkCensorOnDate(cohortTable, censorOnDate)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(outcomeCohortId,
    len = 1,
    add = errorMessage
  )
  checkmate::assert_logical(censorOnCohortExit)
  checkmate::assert_date(censorOnDate, null.ok = TRUE)
  if (followUpDays != Inf) {
    checkmate::assert_integerish(followUpDays,
      len = 1,
      lower = 1,
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
  checkmate::reportAssertions(collection = errorMessage)



  # check specified cohort is in cohort table
  errorMessage <- checkmate::makeAssertCollection()
  if (!is.null(outcomeCohortId)) {
    checkmate::assertTRUE(
      checkCohortId(
        cohort = cdm[[outcomeCohortTable]],
        cohortId = outcomeCohortId
      ),
      add = errorMessage
    )
  }
  return(checkmate::reportAssertions(collection = errorMessage))
}
