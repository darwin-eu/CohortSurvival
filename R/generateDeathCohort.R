
#' To create a death cohort
#'
#' @param cdm  CDM reference
#'
#' @param deathInObservation If TRUE, restricts deaths included to only those
#' observed during an ongoing observation period.
#' @param name name for the created death cohort table
#' @param cohortTable name of the cohort table to create a death cohort for
#' @param cohortId name of the cohort table to create a death cohort for
#'
#' @return A cohort table with a death cohort in cdm
#' @export
#'
#' @examples
generateDeathCohortSet <- function(
    cdm,
    deathInObservation = FALSE,
    name = "death_cohort",
    cohortTable = NULL,
    cohortId = NULL){

  # 0. validate inputs...
  checkCdm(cdm, tables = c(
    "death", "observation_period"
  ))

  checkmate::assertNumeric(cohortId, any.missing = FALSE, null.ok = TRUE)

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
  firstTempTable <- getOption("dbplyr_table_name", 0) + 1

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
      name = paste0(attr(cdm, "write_prefix"), name),
      FALSE, attr(cdm, "write_schema"), TRUE
    )

  cohortSetRef <- cohortRef %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_name = "death_cohort") %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name, "_set"),
      FALSE, attr(cdm, "write_schema"), TRUE
    )

  cohortCountRef <-  cohortRef %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    CDMConnector::computeQuery(
      name = paste0(attr(cdm, "write_prefix"), name, "_count"),
      FALSE, attr(cdm, "write_schema"), TRUE
    )


  cdm[[name]] <- CDMConnector::newGeneratedCohortSet(
    cohortRef = cohortRef,
    cohortSetRef = cohortSetRef,
    cohortCountRef = cohortCountRef
  )

  attr(cdm[[name]], "cohort_attrition") <- tibble::tibble(
    "reason" = "Qualifying initial records",
    "reason_id" = 1,
    "excluded_records" = 0,
    "excluded_subjects" = 0
  )

  # drop intermediary tables that were created in the process
  lastTempTable <- getOption("dbplyr_table_name", 0)
  if (!is.null(attr(cdm, "write_prefix")) & firstTempTable <= lastTempTable) {
    CDMConnector::dropTable(
      cdm, sprintf("dbplyr_%03i", firstTempTable:lastTempTable)
    )
  }

  return(cdm)
}


