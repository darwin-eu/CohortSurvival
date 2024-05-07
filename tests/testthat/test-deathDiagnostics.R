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

  exposure_cohort <- dplyr::tibble(
    subject_id = c(1, 2, 3, 3, 4, 5,6,6),
    cohort_definition_id = c(1, 1, 1, 2, 2, 2,1,2),
    cohort_start_date = c(
      as.Date("2020-01-01"),
      as.Date("2020-02-03"),
      as.Date("2020-05-01"),
      as.Date("2020-05-01"),
      as.Date("2020-08-01"),
      as.Date("2020-09-01"),
      as.Date("2020-09-01"),
      as.Date("2020-09-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2022-02-03"),
      as.Date("2021-06-28"),
      as.Date("2021-06-01"),
      as.Date("2021-08-01"),
      as.Date("2021-09-01"),
      as.Date("2021-09-01"),
      as.Date("2021-09-01")
    )
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list(
      person = person,
      observation_period = observation_period,
      death = deathTable
    ),
    cohortTables = list(
      exposure_cohort = exposure_cohort),
    cdmName = "mock_es"
  )

  cdm2 = CDMConnector::copy_cdm_to(db,
                                   cdm,
                                   schema = "main",
                                   overwrite = TRUE)

  attr(cdm2, "cdm_schema") <- "main"
  attr(cdm2, "write_schema") <- "main"

  diagnostics <- deathDiagnostics(cdm2)

  expect_no_error(omopgenerics::newSummarisedResult(diagnostics))

  expect_true(diagnostics %>%
                dplyr::filter(variable_name == "Not in observation") %>%
                dplyr::pull(estimate_value) == "0")

  expect_true(diagnostics %>%
                dplyr::filter(variable_name == "Future observation" & estimate_name == "mean") %>%
                dplyr::pull(estimate_value) %>% as.numeric() %>% round() == 1235)

  expect_true(diagnostics %>%
                dplyr::filter(variable_name == "Prior observation" & estimate_name == "mean") %>%
                dplyr::pull(estimate_value) %>% as.numeric() %>% round() == 14409)

  diagnostics_cohort <- deathDiagnostics(cdm2,
                                  cohortTable = "exposure_cohort",
                                  cohortId = c(1,2))

  expect_no_error(omopgenerics::newSummarisedResult(diagnostics_cohort))

  expect_true(diagnostics_cohort %>%
                dplyr::filter(variable_name == "Not in observation",
                              group_level == "cohort_1") %>%
                dplyr::pull(estimate_value) == "0")

  expect_true(diagnostics_cohort %>%
                dplyr::filter(variable_name == "Future observation" & estimate_name == "max" & group_level == "cohort_2") %>%
                dplyr::pull(estimate_value) %>% as.numeric()  == 1235)

  expect_true(diagnostics_cohort %>%
                dplyr::filter(variable_name == "Prior observation" & estimate_name == "max" & group_level == "cohort_2") %>%
                dplyr::pull(estimate_value) %>% as.numeric() %>% round() == 14409)

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

  deathTable <- dplyr::tibble(
    person_id = c(1,2,3),
    death_date = c(as.Date("2020-01-01"),
                   as.Date("2020-01-02"),
                   as.Date("2020-01-01")))

  cohort1 <- tibble::tibble(
    cohort_definition_id = c(1,1,1),
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

  cdm3 <- mockMGUS2cdm()

  expect_error(deathDiagnostics(cdm)) # not proper cdm
  expect_error(deathDiagnostics(cdm3)) # no death table
  expect_error(deathDiagnostics(cdm2, cohortTable = "name")) # not a table
  expect_error(deathDiagnostics(cdm2, cohortTable = "cohort1", cohortId = 3)) # not correct cohort id

  CDMConnector::cdmDisconnect(cdm2)
})
