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
  mgus2 <- survival::mgus2 |>
    dplyr::mutate(
      cohort_start_date_diag = as.Date(paste0(
        .data$dxyr, "-01-01"
      )),
      cohort_start_date_progression = clock::add_days(.data$cohort_start_date_diag,
        .data$ptime),
      cohort_start_date_death = clock::add_days(.data$cohort_start_date_diag,
        .data$futime)
    ) |>
    dplyr::rename("subject_id" = "id") |>
    dplyr::mutate(
      observation_period_start_date = clock::add_days(
        .data$cohort_start_date_diag,
        -.data$age)
    ) |>
    dplyr::mutate(subject_id = as.integer(.data$subject_id))


  mgus2Diag <- mgus2 |>
    dplyr::select(
      "subject_id", "cohort_start_date_diag",
      "age", "sex", "hgb", "creat", "mspike"
    ) |>
    dplyr::rename("cohort_start_date" = "cohort_start_date_diag") |>
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      cohort_definition_id = 1L
    ) |>
    dplyr::relocate("cohort_definition_id") |>
    dplyr::relocate("cohort_end_date", .after = "cohort_start_date") |>
    dplyr::mutate(age_group = dplyr::if_else(.data$age < 70, "<70", ">=70"))

  mgus2Diag <- dplyr::as_tibble(mgus2Diag)

  attr(mgus2Diag, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1L, cohort_name = "mgus_diagnosis"
  )
  attr(mgus2Diag, "cohort_attrition") <- addAttrition(mgus2Diag, attr(mgus2Diag, "cohort_set"))


  mgus2Pr <- mgus2 |>
    dplyr::filter(.data$pstat == 1) |>
    dplyr::select("subject_id", "cohort_start_date_progression") |>
    dplyr::rename("cohort_start_date" = "cohort_start_date_progression") |>
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      cohort_definition_id = 1L
    ) |>
    dplyr::relocate("cohort_definition_id")

  mgus2Pr <- dplyr::as_tibble(mgus2Pr)

  attr(mgus2Pr, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1L, cohort_name = "progression"
  )
  attr(mgus2Pr, "cohort_attrition") <- addAttrition(mgus2Pr, attr(mgus2Pr, "cohort_set"))


  mgus2Pr2 <- mgus2 |>
    dplyr::filter(.data$pstat == 1) |>
    dplyr::select("subject_id", "cohort_start_date_progression") |>
    dplyr::rename("cohort_start_date" = "cohort_start_date_progression") |>
    dplyr::mutate(cohort_end_date = .data$cohort_start_date)
  mgus2Pr2 <- mgus2Pr2 |>
    dplyr::mutate(cohort_definition_id = 1L) |>
    dplyr::union_all(
      mgus2Pr2 |>
        dplyr::mutate(cohort_definition_id = 2 + dplyr::row_number() %% 2)
    ) |>
    dplyr::relocate("cohort_definition_id") |>
    dplyr::mutate(cohort_definition_id = as.integer(.data$cohort_definition_id))

  mgus2Pr2 <- dplyr::as_tibble(mgus2Pr2)

  attr(mgus2Pr2, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = c(1L,2L,3L),
    cohort_name = c("any_progression", "progression_type_1", "progression_type_2")
  )
  attr(mgus2Pr2, "cohort_attrition") <- addAttrition(mgus2Pr2, attr(mgus2Pr2, "cohort_set"))

  mgus2Death <- mgus2 |>
    dplyr::filter(.data$death == 1) |>
    dplyr::select("subject_id", "cohort_start_date_death") |>
    dplyr::rename("cohort_start_date" = "cohort_start_date_death") |>
    dplyr::mutate(
      cohort_end_date = .data$cohort_start_date,
      cohort_definition_id = 1L
    ) |>
    dplyr::relocate("cohort_definition_id")

  mgus2Death <- dplyr::as_tibble(mgus2Death)

  attr(mgus2Death, "cohort_set") <- dplyr::tibble(
    cohort_definition_id = 1L, cohort_name = "death_cohort"
  )
  attr(mgus2Death, "cohort_attrition") <- addAttrition(mgus2Death, attr(mgus2Death, "cohort_set"))

  mgus2Person <- mgus2 |>
    dplyr::rename("person_id" = "subject_id") |>
    dplyr::mutate(
      gender_concept_id = dplyr::if_else(
        .data$sex == "F", 8532L, 8507L
      ),
      year_of_birth = as.integer(clock::get_year(mgus2$observation_period_start_date)),
      month_of_birth = as.integer(clock::get_month(mgus2$observation_period_start_date)),
      day_of_birth = clock::get_day(mgus2$observation_period_start_date),
      race_concept_id = 0L,
      ethnicity_concept_id = 0L
    ) |>
    dplyr::select(
      "person_id", "gender_concept_id",
      "year_of_birth", "month_of_birth", "day_of_birth",
      "race_concept_id", "ethnicity_concept_id"
    )

  mgus2OP <- mgus2 |>
    dplyr::rename("person_id" = "subject_id") |>
    dplyr::select(
      "person_id", "observation_period_start_date",
      "cohort_start_date_death"
    ) |>
    dplyr::mutate(
      observation_period_id = sample(c(1:1000000), dim(mgus2)[1]),
      period_type_concept_id = 44814725L
    ) |>
    dplyr::rename("observation_period_end_date" = "cohort_start_date_death")


  # placeholder visit occurrence
  visitOccurrence <- dplyr::tibble(
    visit_occurrence_id = 1001L,
    person_id = 1L,
    visit_concept_id = 5L,
    visit_start_date = as.Date("2020-01-01"),
    visit_end_date = as.Date("2020-01-01"),
    visit_type_concept_id = 44818518L
  )

  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  cdm <- omopgenerics::cdmFromTables(tables = list(
                                       person = mgus2Person,
                                       observation_period = mgus2OP,
                                       visit_occurrence = visitOccurrence
                                       ),
                                     cohortTables = list(
                                       death_cohort = mgus2Death,
                                       mgus_diagnosis = mgus2Diag,
                                       progression = mgus2Pr,
                                       progression_type = mgus2Pr2
                                     ),
                                     cdmName = "mock")

  cdm2 <- CDMConnector::copyCdmTo(db,
                            cdm,
                            schema = "main",
                            overwrite = TRUE)

  # Add schema information
  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  return(cdm2)
}

addAttrition <- function(cohort, set) {
  cohort |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) |>
    dplyr::left_join(
      set |> dplyr::select("cohort_definition_id"),
      by = "cohort_definition_id",
      copy = TRUE
    ) |>
    dplyr::mutate(
      "number_records" = dplyr::if_else(
        is.na(.data$number_records), 0, .data$number_records
      ),
      "number_subjects" = dplyr::if_else(
        is.na(.data$number_subjects), 0, .data$number_subjects
      ),
      "reason_id" = 1L,
      "reason" = "Initial qualifying events",
      "excluded_records" = 0,
      "excluded_subjects" = 0
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      number_records = as.integer(.data$number_records),
      number_subjects = as.integer(.data$number_subjects),
      excluded_records = as.integer(.data$excluded_records),
      excluded_subjects = as.integer(.data$excluded_subjects)
    )
}
