test_that("working example", {
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2020-01-01"),
                          as.Date("2020-01-02"),
                          as.Date("2020-01-01")),
    cohort_end_date = c(as.Date("2020-04-01"),
                        as.Date("2020-08-02"),
                        as.Date("2020-03-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1,1,1),
    person_id = c(1,2,3),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-02"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2023-04-01"),
                                    as.Date("2023-05-02"),
                                    as.Date("2023-03-01"))
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    observation_period = observation_period
  )
  cdm$cohort1 <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1
    )
  expect_true(all(colnames(cdm$cohort1) %in%
    c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date",
      "days_to_exit", "status", "time"
    )))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("another working example", {
  skip_on_cran()

  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  }
  skip_if_not(CDMConnector::eunomia_is_available())

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(con, cdm_schema = "main", write_schema = "main")

  celecoxibCodes <- CodelistGenerator::getDescendants(cdm, conceptId = 1118084)
  cdm$celecoxib <- cdm$drug_era %>%
    dplyr::inner_join(
      celecoxibCodes %>%
        dplyr::select(concept_id),
      by = c("drug_concept_id" = "concept_id"),
      copy = TRUE
    ) %>%
    PatientProfiles::addAge(cdm, indexDate = "drug_era_start_date") %>%
    dplyr::rename(
      "subject_id" = "person_id",
      "cohort_start_date" = "drug_era_start_date",
      "cohort_end_date" = "drug_era_end_date"
    ) %>%
    dplyr::mutate(cohort_definition_id = 1L) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date"
    ) %>%
    CDMConnector::computeQuery(name = "celecoxib", temporary = FALSE, schema = attr(cdm, "write_schema"))
  celecoxib_set <- cdm$celecoxib %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::mutate(cohort_name = "celecoxib")
  celecoxib_count <- cdm$celecoxib %>%
    dplyr::group_by(cohort_definition_id) %>%
    dplyr::tally(name = "n_records")
  celecoxib_count <- cdm$celecoxib %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
  dplyr::collect()

  celecoxib_set <- cdm$celecoxib %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_name = "celecoxib") %>%
    dplyr::collect()

  cdm$celecoxib <- CDMConnector::newGeneratedCohortSet(cohortRef = cdm$celecoxib,
                                                       cohortSetRef = celecoxib_set,
                                                       cohortCountRef = celecoxib_count)


  GiBleedCodes <- CodelistGenerator::getDescendants(cdm, conceptId = 192671)
  cdm$gi_bleed <- cdm$condition_occurrence %>%
    dplyr::inner_join(
      GiBleedCodes %>%
        dplyr::select(concept_id),
      by = c("condition_concept_id" = "concept_id"),
      copy = TRUE
    ) %>%
    dplyr::rename(
      "subject_id" = "person_id",
      "cohort_start_date" = "condition_start_date"
    ) %>%
    dplyr::mutate(cohort_end_date = cohort_start_date) %>%
    dplyr::mutate(cohort_definition_id = 1L) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::compute()
  gi_bleed_count <- cdm$gi_bleed %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    ) %>%
    dplyr::collect()
  gi_bleed_set <- cdm$gi_bleed %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_name = "gi_bleed") %>%
    dplyr::collect()

  cdm$gi_bleed <- CDMConnector::newGeneratedCohortSet(cohortRef = cdm$gi_bleed,
                                                      cohortSetRef = gi_bleed_set,
                                                      cohortCountRef = gi_bleed_count)

  cdm$celecoxib <- cdm$celecoxib %>%
    PatientProfiles::addAge(cdm = cdm) %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "gi_bleed"
    )

  expect_true(all(c("time", "status") %in% colnames(cdm$celecoxib)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("censorOnCohortExit", {
  skip_on_cran()

   cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2020-01-01"),
                          as.Date("2020-01-02"),
                          as.Date("2020-01-01")),
    cohort_end_date = c(as.Date("2020-04-01"),
                        as.Date("2020-08-02"),
                        as.Date("2020-03-01"))
  )
  events <- dplyr::tibble(
    cohort_definition_id = c(1,1,2),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2019-01-01"),
                          as.Date("2020-01-05"),
                          as.Date("2020-01-01")),
    cohort_end_date = c(as.Date("2019-01-01"),
                        as.Date("2020-01-05"),
                        as.Date("2020-01-01")),
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1,1,1),
    person_id = c(1,2,3),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-02"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2023-04-01"),
                                    as.Date("2023-05-02"),
                                    as.Date("2023-03-01"))
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    cohort2 = events,
    observation_period = observation_period
  )
  cohortNoCensorExit <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1
    ) %>%
    dplyr::arrange(subject_id)
  cohortCensorExit <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      censorOnCohortExit = TRUE
    ) %>%
    dplyr::arrange(subject_id)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  expect_true(all(compareNA(cohortNoCensorExit %>%
                    dplyr::select(status) %>%
                    dplyr::pull(),
                    cohortCensorExit %>%
                    dplyr::select(status) %>%
                    dplyr::pull())))
  expect_true(all(compareNA(cohortNoCensorExit %>%
                dplyr::select(time) %>%
                dplyr::pull(),
                c(NA, 3, 1155))))
  expect_true(all(compareNA(cohortNoCensorExit %>%
                          dplyr::select(days_to_exit) %>%
                          dplyr::pull(),
                          c(1186, 1216, 1155))))
  expect_true(all(compareNA(cohortCensorExit %>%
                          dplyr::select(time) %>%
                          dplyr::pull(),
                          c(NA, 3, 60))))
  expect_true(all(compareNA(cohortCensorExit %>%
                          dplyr::select(days_to_exit) %>%
                          dplyr::pull(),
                          c(91, 213, 60))))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("censorOnDate", {
  skip_on_cran()

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2020-01-01"),
                          as.Date("2020-01-02"),
                          as.Date("2021-01-01")),
    cohort_end_date = c(as.Date("2020-04-01"),
                        as.Date("2020-08-02"),
                        as.Date("2021-03-01"))
  )
  events <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2019-01-01"),
                          as.Date("2020-01-05"),
                          as.Date("2021-02-01")),
    cohort_end_date = c(as.Date("2019-01-01"),
                        as.Date("2020-01-05"),
                        as.Date("2021-02-01")),
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1,1,1),
    person_id = c(1,2,3),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-02"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2023-04-01"),
                                    as.Date("2023-05-02"),
                                    as.Date("2023-03-01"))
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    cohort2 = events,
    observation_period = observation_period
  )
  cohortCensorDate <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      censorOnDate = as.Date("2021-01-04")
    ) %>%
    dplyr::arrange(subject_id)

  compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
  }

  expect_true(all(compareNA(cohortCensorDate %>%
                              dplyr::select(status) %>%
                              dplyr::pull(),
                              c(NA, 1, 0))))
  expect_true(all(compareNA(cohortCensorDate %>%
                              dplyr::select(days_to_exit) %>%
                              dplyr::pull(),
                              c(369, 368, 3))))
  expect_true(all(compareNA(cohortCensorDate %>%
                              dplyr::select(time) %>%
                              dplyr::pull(),
                              c(NA, 3, 3))))

  expect_error(
    cdm$cohort1 %>%
      addCohortSurvival(
        cdm = cdm,
        outcomeCohortTable = "cohort2",
        outcomeCohortId = 1,
        censorOnDate = as.Date("2020-01-04")
      )
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("followUpDays", {
  skip_on_cran()

  cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2020-01-01"),
                          as.Date("2020-01-02"),
                          as.Date("2020-01-01")),
    cohort_end_date = c(as.Date("2020-04-01"),
                        as.Date("2020-08-02"),
                        as.Date("2020-01-06"))
  )
  events <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2020-01-30"),
                          as.Date("2020-01-05"),
                          as.Date("2020-01-11")),
    cohort_end_date = c(as.Date("2020-01-30"),
                        as.Date("2020-01-05"),
                        as.Date("2020-01-11")),
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1,1,1),
    person_id = c(1,2,3),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-02"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2023-04-01"),
                                    as.Date("2023-05-02"),
                                    as.Date("2023-03-01"))
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    cohort2 = events,
    observation_period = observation_period
  )
  cohortFollowUp <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      followUp = 20
    ) %>%
    dplyr::arrange(subject_id)
 cohortFUandCE <- cdm$cohort1 %>%
   addCohortSurvival(
     cdm = cdm,
     outcomeCohortTable = "cohort2",
     outcomeCohortId = 1,
     followUp = 20,
     censorOnCohortExit = TRUE
   ) %>%
   dplyr::arrange(subject_id)

 expect_true(all(cohortFollowUp %>%
                   dplyr::select(days_to_exit) %>%
                   dplyr::pull() ==
                   c(20,20,20)))
 expect_true(all(cohortFollowUp %>%
                   dplyr::select(status) %>%
                   dplyr::pull() ==
                   c(0,1,1)))
 expect_true(all(cohortFollowUp %>%
                   dplyr::select(time) %>%
                   dplyr::pull() ==
                   c(20,3,10)))

 expect_true(all(cohortFUandCE %>%
                   dplyr::select(days_to_exit) %>%
                   dplyr::pull() ==
                   c(20,20,5)))
 expect_true(all(cohortFUandCE %>%
                   dplyr::select(status) %>%
                   dplyr::pull() ==
                   c(0,1,0)))
 expect_true(all(cohortFUandCE %>%
                   dplyr::select(time) %>%
                   dplyr::pull() ==
                   c(20,3,5)))

  CDMConnector::cdmDisconnect(cdm)
  })

test_that("expected errors", {
  skip_on_cran()
  cdm <- PatientProfiles::mockPatientProfiles()
  cdm[["cohort1"]] <- cdm[["cohort1"]] %>%
    dplyr::filter(cohort_start_date != "2020-01-01")

  # check outcome cohort
  # id that is not in the table
  expect_error(cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 10
    ))
  # should only work for one cohort
  expect_error(cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = c(1, 2)
    ))
  # user must provide a cohort id
  expect_error(cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = NULL
    ))

  # censorOnCohortExit must be logical
  expect_error(cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      censorOnCohortExit = 1
    ))

  # followUpDays must be 1 or higher
  expect_error(cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      followUpDays = -1
    ))

  expect_error(cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      followUpDays = 0
    ))

  # temporary must be logical
  expect_error(cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      temporary = "maybe"
    ))

  cdm <- PatientProfiles::mockPatientProfiles()
  cdm[["cohort1"]] <- cdm[["cohort1"]] %>%
    dplyr::group_by(subject_id) %>%
    dplyr::filter(dplyr::row_number() == 1)

  # multiple cohort definition ids in exposure table
  expect_error(cdm$cohort1 %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "cohort1",
                   outcomeCohortId = 1
                 ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors2 - index cohort to have one row per person", {
  skip_on_cran()
  cdm <- PatientProfiles::mockPatientProfiles()
  expect_error(cdm$cohort1 %>%
                addCohortSurvival(
                cdm = cdm,
                outcomeCohortTable = "cohort2",
                outcomeCohortId = 1
                ))
  CDMConnector::cdmDisconnect(cdm)
})

test_that("within cohort survival", {
  skip_on_cran()
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2020-01-01"),
                          as.Date("2020-01-02"),
                          as.Date("2020-01-01")),
    cohort_end_date = c(as.Date("2020-04-01"),
                        as.Date("2020-08-02"),
                        as.Date("2021-03-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1,1,1),
    person_id = c(1,2,3),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-02"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2023-04-01"),
                                    as.Date("2023-05-02"),
                                    as.Date("2023-03-01"))
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    observation_period = observation_period
  )

  # default "cohort_start_date"
  # if using the same cohort, status would be 1, time would be 0 for everyone
  cdm$cohort1_start <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1,
      outcomeDateVariable = "cohort_start_date"
    )
  expect_true(all(cdm$cohort1_start %>%
    dplyr::pull("time") == 0))
  expect_true(all(cdm$cohort1_start %>%
                    dplyr::pull("status") == 1))

  cdm$cohort1_end <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1,
      outcomeDateVariable = "cohort_end_date"
    )

  expect_true(all(cdm$cohort1_end %>%
    dplyr::collect() %>%
    dplyr::mutate(dtime = as.numeric(difftime(cohort_end_date,
                                   cohort_start_date)),
                  equal = (dtime == time)) %>%
    dplyr::pull("equal")))

  # limit follow up
  cdm$cohort1_b <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1,
      outcomeDateVariable = "cohort_end_date",
      followUpDays = 100
    )
  expect_true(all(cdm$cohort1_b %>%
                    dplyr::pull("time") == c(91,100,100)))
  expect_true(all(cdm$cohort1_b %>%
                    dplyr::pull("status") == c(1,0,0)))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("allow overwrite of time and status", {
  skip_on_cran()
  cohort <- dplyr::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = c(as.Date("2020-01-01"),
                          as.Date("2020-01-02"),
                          as.Date("2020-01-01")),
    cohort_end_date = c(as.Date("2020-04-01"),
                        as.Date("2020-08-02"),
                        as.Date("2021-03-01"))
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1,1,1),
    person_id = c(1,2,3),
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-02"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2023-04-01"),
                                    as.Date("2023-05-02"),
                                    as.Date("2023-03-01"))
  )
  cdm <- PatientProfiles::mockPatientProfiles(
    cohort1 = cohort,
    observation_period = observation_period
  )

  cdm$cohort1 <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort1",
    )

  cohort1_count <- cdm$cohort1 %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    )
  cohort1_set <- cdm$cohort1 %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_name = "cohort1")

  # currently need to add attribute back to rerun
  attr(cdm$cohort1, "set") <- cohort1_set
  attr(cdm$cohort1, "count") <- cohort1_count

  cdm$cohort1 <- cdm$cohort1 %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "cohort1",
    )
 expect_true(!is.null(cdm$cohort1))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("multiple records per person", {
  skip_on_cran()

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 1, 2, 2, 3),
    cohort_definition_id = rep(1,5),
    cohort_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2015-01-01"),
      as.Date("2010-01-01"),
      as.Date("2016-01-01"),
      as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2010-01-01"),
      as.Date("2015-01-01"),
      as.Date("2010-01-01"),
      as.Date("2016-01-01"),
      as.Date("2010-01-01")
    )
  )
  # outcome during first cohort entry for id 1
  # outcome during second cohort entry for id 2
  outcome_cohort <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = c(
      as.Date("2012-01-10"),
      as.Date("2017-01-10")
    ),
    cohort_end_date = c(
      as.Date("2012-01-10"),
      as.Date("2017-01-10")
    ))
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 1, 1),
    person_id = c(1, 2, 3),
    observation_period_start_date = c(
      as.Date("2007-03-21"),
      as.Date("2006-09-09"),
      as.Date("1980-07-20")
    ),
    observation_period_end_date = c(
      as.Date("2022-09-08"),
      as.Date("2023-01-03"),
      as.Date("2023-05-20")
    )
  )

  cdm <- PatientProfiles::mockPatientProfiles(
    exposure_cohort = exposure_cohort,
    cohort1 = outcome_cohort,
    observation_period = observation_period
  )

  start_rows<-cdm$exposure_cohort %>% dplyr::count() %>% dplyr::pull()

  cdm$exposure_cohort <- cdm$exposure_cohort %>%
    addCohortSurvival(cdm = cdm,
                      outcomeCohortTable = "cohort1")

  end_rows <- cdm$exposure_cohort %>% dplyr::count() %>% dplyr::pull()
  expect_equal(start_rows, end_rows)

  expect_true(cdm$exposure_cohort %>%
    dplyr::filter(subject_id == 1,
                    cohort_start_date == as.Date("2010-01-01")) %>%
    dplyr::pull("status") == 1)
  # NA for the second as their cohort start was after their event
  expect_true(is.na(cdm$exposure_cohort %>%
                dplyr::filter(subject_id == 1,
                              cohort_start_date == as.Date("2015-01-01")) %>%
                dplyr::pull("status")))
  # Both will be status == 1 as event came after second cohort entry
  expect_true(all(cdm$exposure_cohort %>%
                dplyr::filter(subject_id == 2) %>%
                dplyr::pull("status") == 1))
  # no event for subject 3
  expect_true(cdm$exposure_cohort %>%
                    dplyr::filter(subject_id == 3) %>%
                    dplyr::pull("status") == 0)

  CDMConnector::cdm_disconnect(cdm)
  })
