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

#' To create a death cohort
#'
#' @param cdm  CDM reference
#'
#' @param deathInObservation If TRUE, restricts deaths included to only those
#' observed during an ongoing observation period.
#' @param name name for the created death cohort table
#' @param cohortTable name of the cohort table to create a death cohort for
#' @param cohortId name of the cohort table to create a death cohort for
#' @param overwrite	Should the cohort table be overwritten if it already exists?
#'
#' @return A cohort table with a death cohort in cdm
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(CohortSurvival)
#' cdm <- PatientProfiles::mockPatientProfiles()
#' deathTable <- dplyr::tibble(
#'   person_id = c(1,2,3),
#'   death_date = c(as.Date("2020-01-01"),
#'                  as.Date("2020-01-02"),
#'                  as.Date("2020-01-01")))
#' DBI::dbWithTransaction(attr(cdm, "dbcon"), {
#'   DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
#'                     deathTable, overwrite = TRUE)
#' })
#' cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")
#' cdm <- generateDeathCohortSet(cdm=cdm,
#'                               name = "death_cohort")
#' }

generateDeathCohortSet <- function(
    cdm,
    name,
    deathInObservation = FALSE,
    cohortTable = NULL,
    cohortId = NULL,
    overwrite = FALSE){

  # 0. validate inputs...
  checkCdm(cdm, tables = c("death", "observation_period"))
  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertCharacter(name, min.chars = 1, any.missing = FALSE, len = 1)

  # 1. deathInObservation
  if (isTRUE(deathInObservation)){
    x <-  cdm$death %>%
      PatientProfiles::addInObservation(cdm,
                                        indexDate = "death_date") %>%
      dplyr::filter(.data$in_observation==1) %>%
      dplyr::select("person_id", "death_date")
  }else{
    x <-  cdm$death
  }

  x <- x %>%
    dplyr::select("person_id", "death_date") %>%
    dplyr::rename("subject_id" = "person_id")

  # 2. cohortTable and cohortId
  if (!is.null(cohortTable)){
    checkCdm(cdm, tables = c(cohortTable))

    if (!is.null(cohortId)){
      x <- x %>%
        dplyr::inner_join(cdm[[cohortTable]] %>%
                            dplyr::filter(.data$cohort_definition_id %in% cohortId) %>%
                            dplyr::select("subject_id", "cohort_definition_id"),
                          by = c("subject_id")) %>%
        dplyr::select("subject_id", "death_date")

    }else{
      x <- x %>%
        dplyr::inner_join(cdm[[cohortTable]] %>%
                            dplyr::select("subject_id"),
                          by = c("subject_id")) %>%
        dplyr::select("subject_id", "death_date")
    }
  }

  # 3. table ref
  # tables to be deleted
  cohortRef <- x %>%
    dplyr::group_by(.data$subject_id) %>%
    dbplyr::window_order(.data$death_date) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::rename("cohort_start_date" = "death_date") %>%
    dplyr::mutate(cohort_definition_id = 1L ,
                  cohort_end_date = .data$cohort_start_date)  %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    CDMConnector::computeQuery(
      name = name,
      temporary = FALSE,
      schema =  attr(cdm, "write_schema"),
      overwrite = overwrite
    )

  if (is.null(cohortTable)) {
    cohortTable <- as.character(NA)
  }
  if (is.null(cohortId)) {
    cohortId <- as.numeric(NA)
  }
  cohortSetRef <- dplyr::tibble(
    "cohort_definition_id" = 1L,
    "cohort_name" = "death_cohort",
    "death_in_observation" = deathInObservation,
    "cohort_table" = cohortTable,
    "cohort_id" = cohortId
  )

  cdm[[name]] <- CDMConnector::newGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    overwrite = TRUE
  )

  return(cdm)
}


