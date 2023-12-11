test_that("basic example", {
  skip_on_cran()

  cdm <- PatientProfiles::mockPatientProfiles()
  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                     as.Date("2020-01-02"),
                     as.Date("2020-01-01")))
  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")


  cdm <- generateDeathCohortSet(cdm=cdm,
                                name = "death_cohort",
                                overwrite = TRUE)

 expect_true(all(c("cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date") %in%
  colnames(cdm$death_cohort)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("first death record per person", {
  skip_on_cran()
  # check that in the case of multiple death records per person
  # only the first will be used
  cdm <- PatientProfiles::mockPatientProfiles()
  deathTable <- dplyr::tibble(
    person_id = c(1,2,2),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-31")))
  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")

  cdm <- generateDeathCohortSet(cdm=cdm,
                                name = "death_cohort")

  expect_true(nrow(cdm$death_cohort %>%
                     dplyr::filter(subject_id == "2") %>%
                     dplyr::collect()) == 1)


  expect_true(cdm$death_cohort %>%
                    dplyr::filter(subject_id == "2") %>%
                    dplyr::select(cohort_start_date) %>%
                    dplyr::pull() == as.Date("2020-01-02"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test death in observation criteria", {
  skip_on_cran()

  observation_period <- tibble::tibble(
    observation_period_id = c(1, 2),
    person_id = c(1,2),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-01-01"),
      as.Date("2021-01-01")
    )
  )
  cdm <- PatientProfiles::mockPatientProfiles(observation_period = observation_period)

  deathTable <- dplyr::tibble(
    person_id = c(1,2),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02")))
  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")

  cdm <- generateDeathCohortSet(cdm=cdm,
                                name = "death_cohort", deathInObservation = TRUE)

  expect_true(nrow(cdm$death_cohort %>% dplyr::collect()) == 1)

  expect_true(cdm$death_cohort %>%
                    dplyr::select(subject_id) %>%
                    dplyr::pull() == 2)

  expect_true(cdm$death_cohort %>%
                dplyr::select(cohort_start_date) %>%
                dplyr::pull() == as.Date("2020-01-02"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test different cohort table name", {
  skip_on_cran()

  cdm <- PatientProfiles::mockPatientProfiles()

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))
  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")

  cdm <- generateDeathCohortSet(cdm=cdm, name = "my_cohort_death")
  expect_error(CDMConnector::assertTables(cdm, tables=c("death_cohort")))

  expect_no_error(CDMConnector::assertTables(cdm, tables=c("my_cohort_death")))

  expect_true(all(c("cohort_definition_id", "subject_id",
                    "cohort_start_date", "cohort_end_date") %in%
                    colnames(cdm$my_cohort_death)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test subsetting death table by a cohort table", {
  skip_on_cran()

  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1,1,2),
    subject_id = c(1,2,3),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- PatientProfiles::mockPatientProfiles(cohort1 = cohort1)

  deathTable <- dplyr::tibble(
    person_id = seq(1,5),
    death_date = c(as.Date("2020-01-01")))

  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")

  cdm <-  generateDeathCohortSet(cdm=cdm,
                                 name = "death_cohort", cohortTable = "cohort1")

  expect_true(nrow(cdm$death_cohort %>% dplyr::collect()) == 3)

  expect_true(all(cdm$death_cohort %>%
                    dplyr::select(subject_id) %>%
                    dplyr::pull() %in%  c(1,2,3)
                    ))
# with cohortId
  cdm <- PatientProfiles::mockPatientProfiles(cohort1 = cohort1)
  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")

  cdm <-  generateDeathCohortSet(cdm=cdm,
                                 name = "death_cohort", cohortTable = "cohort1", cohortId = 1)

  expect_true(nrow(cdm$death_cohort %>% dplyr::collect()) == 2)

  expect_true(all(cdm$death_cohort %>%
                    dplyr::select(subject_id) %>%
                    dplyr::pull() %in%  c(1,2)
  ))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("test expected errors", {
  skip_on_cran()

  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- PatientProfiles::mockPatientProfiles(cohort1 = cohort1)

  # no death table in CDM
  expect_error(cdm <- generateDeathCohortSet(cdm=cdm,
                                             name = "death_cohort"))

  # cohortTable & cohortId
  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))
  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")

  # cohortTable not exist
  expect_error(cdm <- generateDeathCohortSet(cdm=cdm,
                                             name = "death_cohort", cohortTable = "non_exist_cohort"))

  # wrong cohortId input
  expect_error(cdm <- generateDeathCohortSet(cdm=cdm,
                                             name = "death_cohort", cohortTable = "cohort1", cohortId = "1"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("test single permanent table created", {
  skip_on_cran()

  cdm <- PatientProfiles::mockPatientProfiles()
  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))
  DBI::dbWithTransaction(attr(cdm, "dbcon"), {
    DBI::dbWriteTable(attr(cdm, "dbcon"), "death",
                      deathTable, overwrite = TRUE)
  })
  cdm$death <- dplyr::tbl(attr(cdm, "dbcon"), "death")

  start_tables <- CDMConnector::listTables(attr(cdm, "dbcon"))

  cdm <- generateDeathCohortSet(cdm=cdm,
                                name = "my_death_cohort",
                                overwrite = TRUE)

  end_tables <- CDMConnector::listTables(attr(cdm, "dbcon"))

  testthat::expect_equal(
    sort(end_tables),
                      sort(c(start_tables, "my_death_cohort", "my_death_cohort_set",
                                           "my_death_cohort_count",
                                           "my_death_cohort_attrition")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
