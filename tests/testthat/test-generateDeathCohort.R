test_that("basic example", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5,6),
    person_id = c(1, 2, 3, 4, 5,6),
    observation_period_start_date = c(
      rep(as.Date("1980-07-20"),6)
    ),
    observation_period_end_date = c(
      rep(as.Date("2023-05-20"),6)
    ),
    period_type_concept_id = c(rep(0,6))
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                name = "death_cohort",
                                overwrite = TRUE)

 expect_true(all(c("cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date") %in%
  colnames(cdm2$death_cohort)))

CDMConnector::cdmDisconnect(cdm2)
})

test_that("first death record per person", {
  skip_on_cran()
  # check that in the case of multiple death records per person
  # only the first will be used
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3, 4, 5,6),
    person_id = c(1, 2, 3, 4, 5,6),
    observation_period_start_date = c(
      rep(as.Date("1980-07-20"),6)
    ),
    observation_period_end_date = c(
      rep(as.Date("2023-05-20"),6)
    ),
    period_type_concept_id = c(rep(0,6))
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,2),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-31")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                 name = "death_cohort")

  expect_true(nrow(cdm2$death_cohort %>%
                     dplyr::filter(subject_id == "2") %>%
                     dplyr::collect()) == 1)


  expect_true(cdm2$death_cohort %>%
                    dplyr::filter(subject_id == "2") %>%
                    dplyr::select(cohort_start_date) %>%
                    dplyr::pull() == as.Date("2020-01-02"))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("test death in observation criteria", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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
    ),
    period_type_concept_id = c(rep(0,2))
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                 name = "death_cohort",
                                 deathInObservation = TRUE)

  expect_true(nrow(cdm2$death_cohort %>% dplyr::collect()) == 1)

  expect_true(cdm2$death_cohort %>%
                    dplyr::select(subject_id) %>%
                    dplyr::pull() == 2)

  expect_true(cdm2$death_cohort %>%
                dplyr::select(cohort_start_date) %>%
                dplyr::pull() == as.Date("2020-01-02"))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("test different cohort table name", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  observation_period <- tibble::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1,2, 3),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3))
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                 name = "my_cohort_death")

  expect_error(CDMConnector::assertTables(cdm2, tables=c("death_cohort")))

  expect_no_error(CDMConnector::assertTables(cdm2, tables=c("my_cohort_death")))

  expect_true(all(c("cohort_definition_id", "subject_id",
                    "cohort_start_date", "cohort_end_date") %in%
                    colnames(cdm2$my_cohort_death)))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("test subsetting death table by a cohort table", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  observation_period <- tibble::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1,2, 3),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3))
  )

  deathTable <- dplyr::tibble(
    person_id = seq(1,5),
    death_date = c(as.Date("2020-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1,1,2),
    subject_id = c(1,2,3),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cohortTables = list(
      cohort1 = cohort1
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                 name = "death_cohort",
                                 cohortTable = "cohort1")

  expect_true(nrow(cdm2$death_cohort %>% dplyr::collect()) == 3)

  expect_true(all(cdm2$death_cohort %>%
                    dplyr::select(subject_id) %>%
                    dplyr::pull() %in%  c(1,2,3)
                    ))
# with cohortId

  cdm2 <-  generateDeathCohortSet(cdm=cdm2,
                                 name = "death_cohort",
                                 cohortTable = "cohort1",
                                 cohortId = 1,
                                 overwrite = TRUE)

  expect_true(nrow(cdm2$death_cohort %>% dplyr::collect()) == 2)

  expect_true(all(cdm2$death_cohort %>%
                    dplyr::select(subject_id) %>%
                    dplyr::pull() %in%  c(1,2)
  ))
  CDMConnector::cdmDisconnect(cdm2)

})

test_that("test expected errors", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  observation_period <- tibble::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1,2, 3),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3))
  )

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1,1,1),
    subject_id = c(1,2,3),
    cohort_start_date = as.Date(c("2012-02-01")),
    cohort_end_date = as.Date(c("2013-02-01"))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period
    ),
    cohortTables = list(
      cohort1 = cohort1
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"


  # no death table in CDM
  expect_error(cdm <- generateDeathCohortSet(cdm=cdm2,
                                             name = "death_cohort"))

  # cohortTable & cohortId
  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))


  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cohortTables = list(
      cohort1 = cohort1
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  # cohortTable not exist
  expect_error(cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                             name = "death_cohort", cohortTable = "non_exist_cohort"))

  # wrong cohortId input
  expect_error(cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                             name = "death_cohort", cohortTable = "cohort1", cohortId = "1"))

  CDMConnector::cdmDisconnect(cdm2)
})

test_that("test single permanent table created", {
  skip_on_cran()
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  observation_period <- tibble::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1,2, 3),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2021-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-01-01"),
      as.Date("2021-01-01"),
      as.Date("2022-01-01")
    ),
    period_type_concept_id = c(rep(0,3))
  )

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2021-01-01")))

  person <- dplyr::tibble(
    person_id = c(1, 2, 3, 4, 5),
    year_of_birth = c(rep("1990", 5)),
    month_of_birth = c(rep("02", 5)),
    day_of_birth = c(rep("11", 5)),
    gender_concept_id = c(rep(0,5)),
    ethnicity_concept_id = c(rep(0,5)),
    race_concept_id = c(rep(0,5))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  start_tables <- attr(cdm2, "names")

  cdm2 <- generateDeathCohortSet(cdm=cdm2,
                                name = "my_death_cohort",
                                overwrite = TRUE)

  end_tables <- attr(cdm2, "names")

  testthat::expect_equal(
    sort(end_tables),
                      sort(c(start_tables, "my_death_cohort")))

  CDMConnector::cdmDisconnect(cdm2)
})
