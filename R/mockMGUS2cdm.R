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

#' Create mock CDM reference with survival::mgus2 dataset
#'
#' @return CDM reference containing data from the survival::mgus2 dataset
#' @export
#'
#' @examples
#'  \donttest{
#' cdm <- mockMGUS2cdm()
#' cdm$person
#' }
mockMGUS2cdm <- function() {
  mgus2 <- survival::mgus2 %>%
    dplyr::mutate(
      cohort_start_date_diag = as.Date(paste0(
        .data$dxyr, "-01-01"
      )),
      cohort_start_date_progression = .data$cohort_start_date_diag +
        lubridate::days(.data$ptime),
      cohort_start_date_death = .data$cohort_start_date_diag +
        lubridate::days(.data$futime)
    ) %>%
    dplyr::rename("subject_id" = "id") %>%
    dplyr::mutate(
      observation_period_start_date =
        .data$cohort_start_date_diag -
        lubridate::years(.data$age)
    )

  mgus2Diag <- mgus2 %>%
    dplyr::select(
      "subject_id", "cohort_start_date_diag",
      "age", "sex", "hgb", "creat", "mspike"
    ) %>%
    dplyr::rename("cohort_start_date" = "cohort_start_date_diag") %>%
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      cohort_definition_id = 1L
    ) %>%
    dplyr::relocate("cohort_definition_id") %>%
    dplyr::relocate("cohort_end_date", .after = "cohort_start_date") %>%
    dplyr::mutate(age_group = dplyr::if_else(.data$age < 70, "<70", ">=70"))

  mgus2Pr <- mgus2 %>%
    dplyr::filter(.data$pstat == 1) %>%
    dplyr::select("subject_id", "cohort_start_date_progression") %>%
    dplyr::rename("cohort_start_date" = "cohort_start_date_progression") %>%
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      cohort_definition_id = 1L
    ) %>%
    dplyr::relocate("cohort_definition_id")

  mgus2Pr2 <- mgus2 %>%
    dplyr::filter(.data$pstat == 1) %>%
    dplyr::select("subject_id", "cohort_start_date_progression") %>%
    dplyr::rename("cohort_start_date" = "cohort_start_date_progression") %>%
    dplyr::mutate(cohort_end_date = .data$cohort_start_date)
  mgus2Pr2 <- mgus2Pr2 %>%
    dplyr::mutate(cohort_definition_id = 1L) %>%
    dplyr::union_all(
      mgus2Pr2 %>%
        dplyr::mutate(cohort_definition_id = 2 + dplyr::row_number() %% 2)
    ) %>%
    dplyr::relocate("cohort_definition_id")

  mgus2Death <- mgus2 %>%
    dplyr::filter(.data$death == 1) %>%
    dplyr::select("subject_id", "cohort_start_date_death") %>%
    dplyr::rename("cohort_start_date" = "cohort_start_date_death") %>%
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      cohort_definition_id = 1L
    ) %>%
    dplyr::relocate("cohort_definition_id")

  mgus2Person <- mgus2 %>%
    dplyr::rename("person_id" = "subject_id") %>%
    dplyr::mutate(
      gender_concept_id = dplyr::if_else(
        .data$sex == "F", 8532, 8507
      ),
      year_of_birth = lubridate::year(mgus2$observation_period_start_date),
      month_of_birth = lubridate::month(mgus2$observation_period_start_date),
      day_of_birth = lubridate::day(mgus2$observation_period_start_date)
    ) %>%
    dplyr::select(
      "person_id", "gender_concept_id",
      "year_of_birth", "month_of_birth", "day_of_birth"
    )

  mgus2OP <- mgus2 %>%
    dplyr::rename("person_id" = "subject_id") %>%
    dplyr::select(
      "person_id", "observation_period_start_date",
      "cohort_start_date_death"
    ) %>%
    dplyr::rename("observation_period_end_date" = "cohort_start_date_death")


  # placeholder visit occurrence
  visitOccurrence <- dplyr::tibble(
    visit_occurrence_id = 1001,
    person_id = 1,
    visit_concept_id = 5,
    visit_start_date = c("2020-01-01"),
    visit_end_date = c("2020-01-01")
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # person
  DBI::dbWriteTable(db, "person", mgus2Person, overwrite = TRUE)

  # obs
  DBI::dbWriteTable(db, "observation_period", mgus2OP, overwrite = TRUE)

  # cohort diag
  DBI::dbWriteTable(db, "mgus_diagnosis", mgus2Diag, overwrite = TRUE)
  DBI::dbWriteTable(db, "mgus_diagnosis_set", dplyr::tibble(
    cohort_definition_id = 1, cohort_name = "mgus_diagnosis"
  ), overwrite = TRUE)

  # cohort progression
  DBI::dbWriteTable(db,  "progression", mgus2Pr, overwrite = TRUE)
  DBI::dbWriteTable(db, "progression_set", dplyr::tibble(
    cohort_definition_id = 1, cohort_name = "progression"
  ), overwrite = TRUE)

  # cohort progression types
  DBI::dbWriteTable(db,  "progression_type", mgus2Pr2, overwrite = TRUE)
  DBI::dbWriteTable(db, "progression_type_set", dplyr::tibble(
    cohort_definition_id = 1:3,
    cohort_name = c("any_progression", "progression_type1", "progression_type2")
  ), overwrite = TRUE)

  # cohort death
  DBI::dbWriteTable(db, "death_cohort", mgus2Death, overwrite = TRUE)
  DBI::dbWriteTable(db, "death_cohort_set", dplyr::tibble(
    cohort_definition_id = 1, cohort_name = "death_cohort"
  ), overwrite = TRUE)

  DBI::dbWriteTable(db, "visit_occurrence", visitOccurrence, overwrite = TRUE)

  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cohort_tables = c("mgus_diagnosis", "progression", "progression_type", "death_cohort"),
    cdm_schema = "main",
    write_schema = "main",
    cdm_name = "mock"
  )

  return(cdm)
}
