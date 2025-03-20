test_that("working example", {
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
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
                                    as.Date("2023-03-01")),

    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(2, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copyCdmTo(db,
                                 cdm,
                                 schema = "main",
                                 overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # test name argument works
  cdm2$cohort2 <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1,
      name = "cohort2"
    )

  cdm2$cohort1 <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1
    )
  expect_true(all(colnames(cdm2$cohort1) %in%
                    c(
                      "cohort_definition_id", "subject_id",
                      "cohort_start_date", "cohort_end_date",
                      "days_to_exit", "status", "time"
                    )))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("another working example", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
    Sys.setenv("EUNOMIA_DATA_FOLDER" = tempdir())
  }
  skip_if_not(CDMConnector::eunomiaIsAvailable())

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  celecoxibCodes <- CodelistGenerator::getDescendants(cdm, conceptId = 1118084)
  cdm$celecoxib <- cdm$drug_exposure %>%
    dplyr::inner_join(
      celecoxibCodes %>%
        dplyr::select(concept_id),
      by = c("drug_concept_id" = "concept_id"),
      copy = TRUE
    ) %>%
    PatientProfiles::addDemographics(indexDate = "drug_exposure_start_date") %>%
    dplyr::filter(!is.na(prior_observation) &
                    prior_observation>=0 ) %>%
    dplyr::rename(
      "subject_id" = "person_id",
      "cohort_start_date" = "drug_exposure_start_date"
    ) %>%
    dplyr::mutate(cohort_definition_id = 1L,
                  cohort_end_date = .data$cohort_start_date) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::compute(name = "celecoxib",
                   temporary = FALSE)

  celecoxib_set <- cdm$celecoxib %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_name = "celecoxib") %>%
    dplyr::collect()

  cdm$celecoxib <- cdm$celecoxib %>%
    omopgenerics::newCohortTable(cohortSetRef =  celecoxib_set)


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
    dplyr::compute(temporary = FALSE,
                   name = "gi_bleed")
  gi_bleed_set <- cdm$gi_bleed %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_name = "gi_bleed") %>%
    dplyr::collect()

  cdm$gi_bleed <- cdm$gi_bleed %>%
    omopgenerics::newCohortTable(cohortSetRef = gi_bleed_set)

  cdm$celecoxib <- cdm$celecoxib %>%
    PatientProfiles::addAge() %>%
    addCohortSurvival(
      cdm = cdm,
      outcomeCohortTable = "gi_bleed"
    )

  expect_true(all(c("time", "status") %in% colnames(cdm$celecoxib)))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("censorOnCohortExit", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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
                                    as.Date("2023-03-01")),
    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(2, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort,
      cohort2 = events
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copyCdmTo(db,
                                 cdm,
                                 schema = "main",
                                 overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cohortNoCensorExit <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1
    ) %>%
    dplyr::arrange(subject_id)
  cohortCensorExit <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
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
  expect_true(all(compareNA(sort(cohortNoCensorExit %>%
                                   dplyr::select(time) %>%
                                   dplyr::pull()),
                            sort(c(NA, 3, 1155)))))
  expect_true(all(compareNA(sort(cohortNoCensorExit %>%
                                   dplyr::select(days_to_exit) %>%
                                   dplyr::pull()),
                            sort(c(1186, 1216, 1155)))))
  expect_true(all(compareNA(sort(cohortCensorExit %>%
                                   dplyr::select(time) %>%
                                   dplyr::pull()),
                            sort(c(NA, 3, 60)))))
  expect_true(all(compareNA(sort(cohortCensorExit %>%
                                   dplyr::select(days_to_exit) %>%
                                   dplyr::pull()),
                            sort(c(91, 213, 60)))))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("censorOnDate", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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
                                    as.Date("2023-03-01")),
    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(2, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort,
      cohort2 = events
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copyCdmTo(db,
                                 cdm,
                                 schema = "main",
                                 overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cohortCensorDate <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
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

  expect_true(all(compareNA(sort(cohortCensorDate %>%
                                   dplyr::select(status) %>%
                                   dplyr::pull()),
                            sort(c(NA, 1, 0)))))
  expect_true(all(compareNA(sort(cohortCensorDate %>%
                                   dplyr::select(days_to_exit) %>%
                                   dplyr::pull()),
                            sort(c(369, 368, 3)))))
  expect_true(all(compareNA(sort(cohortCensorDate %>%
                                   dplyr::select(time) %>%
                                   dplyr::pull()),
                            sort(c(NA, 3, 3)))))

  expect_error(
    cdm2$cohort1 %>%
      addCohortSurvival(
        cdm = cdm,
        outcomeCohortTable = "cohort2",
        outcomeCohortId = 1,
        censorOnDate = as.Date("2020-01-04")
      )
  )

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("followUpDays", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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
                                    as.Date("2023-03-01")),
    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(2, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort,
      cohort2 = events
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copyCdmTo(db,
                                 cdm,
                                 schema = "main",
                                 overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cohortFollowUp <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort2",
      outcomeCohortId = 1,
      followUp = 20
    ) %>%
    dplyr::arrange(subject_id)
  cohortFUandCE <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
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

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("expected errors", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  # check outcome cohort
  # id that is not in the table
  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = 2
                 ))
  # should only work for one cohort
  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = c(1, 2)
                 ))
  # user must provide a cohort id
  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = NULL
                 ))

  # censorOnCohortExit must be logical
  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = 1,
                   censorOnCohortExit = 1
                 ))

  # followUpDays must be 1 or higher
  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = 1,
                   followUpDays = -1
                 ))

  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = 1,
                   followUpDays = 0
                 ))

  # temporary must be logical
  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = 1,
                   temporary = "maybe"
                 ))

  expect_error(cdm$mgus_diagnosis %>%
                 addCohortSurvival(
                   cdm = cdm,
                   outcomeCohortTable = "death_cohort",
                   outcomeCohortId = 1,
                   outcomeWashout = c(0,1)
                 ))

  #  cdm <- PatientProfiles::mockPatientProfiles()
  #  cdm[["cohort1"]] <- cdm[["cohort1"]] %>%
  #    dplyr::group_by(subject_id) %>%
  #    dplyr::filter(dplyr::row_number() == 1)

  # multiple cohort definition ids in exposure table
  #  expect_error(cdm$cohort1 %>%
  #                 addCohortSurvival(
  #                   cdm = cdm,
  #                   outcomeCohortTable = "cohort1",
  #                   outcomeCohortId = 1
  #                 ))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("within cohort survival", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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
                                    as.Date("2023-03-01")),
    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(2, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copyCdmTo(db,
                                 cdm,
                                 schema = "main",
                                 overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # default "cohort_start_date"
  # if using the same cohort, status would be 1, time would be 0 for everyone
  cdm2$cohort1_start <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1,
      outcomeDateVariable = "cohort_start_date"
    )
  expect_true(all(cdm2$cohort1_start %>%
                    dplyr::pull("time") == 0))
  expect_true(all(cdm2$cohort1_start %>%
                    dplyr::pull("status") == 1))

  cdm2$cohort1_end <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1,
      outcomeDateVariable = "cohort_end_date"
    )

  expect_true(all(cdm2$cohort1_end %>%
                    dplyr::collect() %>%
                    dplyr::mutate(dtime = as.numeric(difftime(cohort_end_date,
                                                              cohort_start_date)),
                                  equal = (dtime == time)) %>%
                    dplyr::pull("equal")))

  # limit follow up
  cdm2$cohort1_b <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort1",
      outcomeCohortId = 1,
      outcomeDateVariable = "cohort_end_date",
      followUpDays = 100
    )
  expect_true(all(cdm2$cohort1_b %>%
                    dplyr::pull("time") == c(91,100,100)))
  expect_true(all(cdm2$cohort1_b %>%
                    dplyr::pull("status") == c(1,0,0)))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("allow overwrite of time and status", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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
                                    as.Date("2023-03-01")),
    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(2, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copyCdmTo(db,
                                 cdm,
                                 schema = "main",
                                 overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cdm2$cohort1 <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort1",
    )

  cohort1_count <- cdm2$cohort1 %>%
    dplyr::group_by(.data$cohort_definition_id) %>%
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id),
      .groups = "drop"
    )
  cohort1_set <- cdm2$cohort1 %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(cohort_name = "cohort1")

  # currently need to add attribute back to rerun
  attr(cdm2$cohort1, "set") <- cohort1_set
  attr(cdm2$cohort1, "count") <- cohort1_count
  attr(cdm2$cohort1, "tbl_name") <- "cohort1"

  cdm2$cohort1 <- cdm2$cohort1 %>%
    addCohortSurvival(
      cdm = cdm2,
      outcomeCohortTable = "cohort1",
    )
  expect_true(!is.null(cdm2$cohort1))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("multiple records per person", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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
    ),
    period_type_concept_id = c(rep(0,3))
  )
  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep(1990, 5)),
    month_of_birth = c(rep(2, 5)),
    day_of_birth = c(rep(11, 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = outcome_cohort,
      exposure_cohort = exposure_cohort
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copyCdmTo(db,
                                 cdm,
                                 schema = "main",
                                 overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  start_rows<-cdm2$exposure_cohort %>% dplyr::count() %>% dplyr::pull()

  cdm2$exposure_cohort <- cdm2$exposure_cohort %>%
    addCohortSurvival(cdm = cdm2,
                      outcomeCohortTable = "cohort1")

  end_rows <- cdm2$exposure_cohort %>% dplyr::count() %>% dplyr::pull()
  expect_equal(start_rows, end_rows)

  expect_true(cdm2$exposure_cohort %>%
                dplyr::filter(subject_id == 1,
                              cohort_start_date == as.Date("2010-01-01")) %>%
                dplyr::pull("status") == 1)
  # NA for the second as their cohort start was after their event
  expect_true(is.na(cdm2$exposure_cohort %>%
                      dplyr::filter(subject_id == 1,
                                    cohort_start_date == as.Date("2015-01-01")) %>%
                      dplyr::pull("status")))
  # Both will be status == 1 as event came after second cohort entry
  expect_true(all(cdm2$exposure_cohort %>%
                    dplyr::filter(subject_id == 2) %>%
                    dplyr::pull("status") == 1))
  # no event for subject 3
  expect_true(cdm2$exposure_cohort %>%
                dplyr::filter(subject_id == 3) %>%
                dplyr::pull("status") == 0)

  CDMConnector::cdmDisconnect(cdm2)
})
