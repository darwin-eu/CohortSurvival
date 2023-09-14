#' Create mock CDM reference with survival::mgus2 dataset
#'
#' @return CDM reference containing data from the survival::mgus2 dataset
#' @export
#'
#' @examples
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
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person",
                      mgus2Person,
                      overwrite = TRUE
    )
  })

  # obs
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period",
                      mgus2OP,
                      overwrite = TRUE
    )
  })

  # cohort diag
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "mgus_diagnosis",
                      mgus2Diag,
                      overwrite = TRUE
    )
  })

  # cohort progression
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db,  "progression",
                      mgus2Pr,
                      overwrite = TRUE
    )
  })

  # cohort death
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "death_cohort",
                      mgus2Death,
                      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "visit_occurrence",
                      visitOccurrence,
                      overwrite = TRUE
    )
  })


  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cohort_tables = c(
      "mgus_diagnosis", "progression",
      "death_cohort"
    ),
    cdm_schema = "main",
    write_schema = "main",
    cdm_name = "mock"
  )

  cdm$mgus_diagnosis <- addCohortCountAttr(cdm$mgus_diagnosis,
                                           name="mgus_diagnosis")
  cdm$progression <- addCohortCountAttr(cdm$progression,
                                        name="progression")
  cdm$death_cohort <- addCohortCountAttr(cdm$death_cohort,
                                         name="death_cohort")

  return(cdm)
}


#' Function to add attributes to cohort table
#' it adds cohort_count, cohort_set, cohort_count, cohort_attrition
#'
#' @noRd
#'
addCohortCountAttr <- function(cohort, name = "cohort") {
  cohortCount <- cohort %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) %>%
    dplyr::collect()

  attr(cohort, "cohort_count") <- cohortCount
  attr(cohort, "cohort_set") <- cohortCount %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::mutate(cohort_name = .env$name)

  attr(cohort, "cohort_attrition") <- cohortCount %>%
    dplyr::mutate(
      "reason" = "Qualifying initial records",
      "reason_id" = 1,
      "excluded_records" = 0,
      "excluded_subjects" = 0
    )

  return(cohort)
}
