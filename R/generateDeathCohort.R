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
#' `r lifecycle::badge("deprecated")`
#'
#' @param cdm  CDM reference
#'
#' @param name name for the created death cohort table
#' @param cohortTable name of the cohort table to create a death cohort for
#' @param cohortId name of the cohort table to create a death cohort for
#'
#' @return A cohort table with a death cohort in cdm
#' @export
#'
#' @examples
#' \donttest{
#' library(CDMConnector)
#' library(CohortSurvival)
#' observation_period <- dplyr::tibble(
#'   observation_period_id = c(1, 2, 3, 4, 5,6),
#'   person_id = c(1, 2, 3, 4, 5,6),
#'   observation_period_start_date = c(
#'     rep(as.Date("1980-07-20"),6)
#'   ),
#'   observation_period_end_date = c(
#'     rep(as.Date("2023-05-20"),6)
#'   ),
#'   period_type_concept_id = c(rep(0,6))
#' )
#'
#' deathTable <- dplyr::tibble(
#'   person_id = c(1,2,3),
#'   death_date = c(as.Date("2020-01-01"),
#'                  as.Date("2020-01-02"),
#'                  as.Date("2020-01-01")))
#'
#' person <- dplyr::tibble(
#'   person_id = c(1, 2, 3, 4, 5),
#'   year_of_birth = c(rep("1990", 5)),
#'   month_of_birth = c(rep("02", 5)),
#'   day_of_birth = c(rep("11", 5)),
#'   gender_concept_id = c(rep(0,5)),
#'   ethnicity_concept_id = c(rep(0,5)),
#'   race_concept_id = c(rep(0,5))
#' )
#'
#' cdm <- omopgenerics::cdmFromTables(
#'   tables = list(
#'     person = person,
#'     observation_period = observation_period,
#'     death = deathTable
#'   ),
#'   cdmName = "mock_es"
#' )
#'  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' cdm2 = CDMConnector::copy_cdm_to(db,
#'                                  cdm,
#'                                  schema = "main")
#'
#' attr(cdm2, "cdm_schema") <- "main"
#' attr(cdm2, "write_schema") <- "main"
#'
#' cdm2 <- generateDeathCohortSet(cdm=cdm2,
#'                                name = "death_cohort")
#' }

generateDeathCohortSet <- function(
    cdm,
    name,
    cohortTable = NULL,
    cohortId = NULL){

  lifecycle::deprecate_soft(
    "0.6.0", "generateDeathCohortSet()"
  )

  # 0. validate inputs...
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertTable(cdm[["death"]])
  omopgenerics::assertNumeric(cohortId, null = TRUE, length = 1)
  omopgenerics::assertCharacter(name, length = 1)

  x <-  cdm$death %>%
    PatientProfiles::addInObservation(indexDate = "death_date") %>%
    dplyr::filter(.data$in_observation==1) %>%
    dplyr::select("subject_id" = "person_id",
                  "death_date")

  # 1. cohortTable and cohortId
  if (!is.null(cohortTable)){
    omopgenerics::validateCdmArgument(cdm)
    omopgenerics::validateCohortArgument(cdm[[cohortTable]])

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

  # 2. table ref
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
    dplyr::ungroup() %>%
    dplyr::compute(
      name = name,
      temporary = FALSE,
      overwrite = TRUE)

  attr(cohortRef, "tbl_name") <- name

  if (is.null(cohortTable)) {
    cohortTable <- as.character(NA)
  }
  if (is.null(cohortId)) {
    cohortId <- as.numeric(NA)
  }
  cohortSetRef <- dplyr::tibble(
    "cohort_definition_id" = 1L,
    "cohort_name" = "death_cohort",
    "cohort_table" = cohortTable,
    "cohort_id" = cohortId
  )

  cdm[[name]] <- cohortRef %>%
    omopgenerics::newCohortTable(cohortSetRef = cohortSetRef)

  return(cdm)
}
