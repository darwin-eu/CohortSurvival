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

checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      ntables <- length(tables)
      cli::cli_abort(paste0(
        "{(ntables)} table{?s} {?is/are} not present in the cdm object: ",
        paste0(tables, collapse = ", ")
      ))
    }
  }
  invisible(NULL)
}

checkIsCdmTable <- function(cdmTable) {
  isCdmTable <- all(c("person_id") %in%
    colnames(cdmTable))

  return(isCdmTable)
}

checkIsCohort <- function(cohort) {
  isCohort <- all(c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    colnames(cohort)) &
    !is.null(attr(cohort, "cohort_set"))

  if (isFALSE(isCohort)) {
    return(cli::cli_abort(c(
      "cohort must be a cohort table with cohort attributes"
    )))
  } else {
    return(invisible(isCohort))
  }
}

checkCohortId <- function(cohort, cohortId) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(cohortId,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  cohortIdPresent <- list()
  for (i in seq_along(cohortId)) {
    workingId <- cohortId[[i]]
    cohortIdPresent[[i]] <- (cohort %>%
      dplyr::filter(.data$cohort_definition_id == .env$workingId) %>%
      dplyr::tally() %>%
      dplyr::pull("n") > 0)
  }
  cohortIdPresent <- all(unlist(cohortIdPresent))
  return(cohortIdPresent)
}

checkStrata <- function(strata, x) {
  checkmate::assertList(
    strata,
    any.missing = FALSE, unique = TRUE, min.len = 1, null.ok = TRUE
  )
  namesInColumns <- all(unlist(strata) %>% unique() %in% colnames(x))
  if(!isTRUE(namesInColumns)) {
    return(cli::cli_abort(c(
      "the cohort table must contain all variables in the strata list as columns"
    )))
  }
}

checkExposureCohortId <- function(cohort) {
  isCohortIdUnique <- length(cohort %>%
                          dplyr::select("cohort_definition_id") %>%
                          dplyr::pull() %>%
                          unique()) == 1

  if(isFALSE(isCohortIdUnique)) {
    return(cli::cli_abort(c(
      "the exposure cohort must only have one id in cohort_definition_id in addSurvival stage"
    )))
  }
}


checkIsCohort_exp <- function(cohort) {
  isCohort <- all(c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  ) %in% colnames(cohort))
  if (isFALSE(isCohort)) {
    return(cli::cli_abort(c(
      "{cohort} must be a cohort table" # This gives a very ugly error right now
    )))
  } else {
    return(invisible(isCohort))
  }
}

checkCensorOnDate <- function(cohort, censorOnDate) {
  if(!is.null(censorOnDate)) {
    start_dates <- cohort %>%
      dplyr::select("cohort_start_date") %>%
      dplyr::pull()
    if(max(start_dates) > censorOnDate) {
      return(cli::cli_abort(c(
        "the target cohort has at least one cohort_start_date after the censor date {censorOnDate}"
      )))
    }
  }
}
