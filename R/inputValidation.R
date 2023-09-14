
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

checkPatientRows <- function(cohort) {
  oneRowperPatient <- cohort %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::mutate(num = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$num > 1) %>%
    dplyr::tally() %>%
    dplyr::pull()

  if (oneRowperPatient > 0) {
    return(cli::cli_abort(c(
      "the cohort table must only contain one row per patient"
    )))
  } else {
    return(invisible())
  }
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
