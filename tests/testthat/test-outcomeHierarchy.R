test_that("censorTie is applied consistently to every censoring boundary", {
  skip_on_cran()

  target <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1:4,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date(c("2020-01-11", "2020-01-11", "2020-01-21", "2020-01-21"))
  )
  outcome <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1:4,
    cohort_start_date = as.Date("2020-01-11"),
    cohort_end_date = as.Date("2020-01-11")
  )
  observation_period <- dplyr::tibble(
    observation_period_id = 1:4,
    person_id = 1:4,
    observation_period_start_date = as.Date("2019-01-01"),
    observation_period_end_date = as.Date(c("2020-01-11", "2020-02-01", "2020-02-01", "2020-02-01")),
    period_type_concept_id = 0L
  )
  person <- dplyr::tibble(
    person_id = 1:4,
    year_of_birth = 1990L,
    month_of_birth = 1L,
    day_of_birth = 1L,
    gender_concept_id = 0L,
    ethnicity_concept_id = 0L,
    race_concept_id = 0L
  )

  cdm <- mockCohortSurvival(
    tables = list(person = person, observation_period = observation_period),
    cohortTables = list(target = target, outcome = outcome),
    cdmName = "hierarchy"
  )

  status_at <- function(subjectId, censorTie = "event", ...) {
    cdm$target |>
      dplyr::filter(.data$subject_id == .env$subjectId) |>
      addCohortSurvival(
        cdm = cdm,
        outcomeCohortTable = "outcome",
        outcomeCohortId = 1,
        outcomeWashout = 0,
        censorTie = censorTie,
        ...
      ) |>
      dplyr::pull("status")
  }

  # End of observation period.
  expect_equal(status_at(1), 1)
  expect_equal(status_at(1, censorTie = "censor"), 0)

  # Target cohort exit.
  expect_equal(status_at(2, censorOnCohortExit = TRUE), 1)
  expect_equal(status_at(2, censorTie = "censor", censorOnCohortExit = TRUE), 0)

  # Explicit censoring date.
  expect_equal(status_at(3, censorOnDate = as.Date("2020-01-11")), 1)
  expect_equal(
    status_at(3, censorTie = "censor", censorOnDate = as.Date("2020-01-11")),
    0
  )

  # Fixed follow-up boundary.
  expect_equal(status_at(4, followUpDays = 10), 1)
  expect_equal(status_at(4, censorTie = "censor", followUpDays = 10), 0)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("outcomeTie resolves same-day outcome and competing outcome", {
  data <- tibble::tibble(
    outcome_time = 10,
    outcome_status = 1,
    competing_time = 10,
    competing_status = 1
  )

  outcome_first <- addCompetingRiskVars(
    data,
    time1 = "outcome_time",
    status1 = "outcome_status",
    time2 = "competing_time",
    status2 = "competing_status",
    nameOutTime = "time",
    nameOutStatus = "status"
  )
  competing_first <- addCompetingRiskVars(
    data,
    time1 = "outcome_time",
    status1 = "outcome_status",
    time2 = "competing_time",
    status2 = "competing_status",
    nameOutTime = "time",
    nameOutStatus = "status",
    outcomeTie = "competingOutcome"
  )

  expect_identical(as.character(outcome_first$status), "1")
  expect_identical(as.character(competing_first$status), "2")
  expect_equal(outcome_first$time, 10)
  expect_equal(competing_first$time, 10)
})

test_that("three-way ties apply censorTie before outcomeTie", {
  censored <- tibble::tibble(
    outcome_time = 10,
    outcome_status = 0,
    competing_time = 10,
    competing_status = 0
  ) |>
    addCompetingRiskVars(
      time1 = "outcome_time",
      status1 = "outcome_status",
      time2 = "competing_time",
      status2 = "competing_status",
      nameOutTime = "time",
      nameOutStatus = "status",
      outcomeTie = "competingOutcome"
    )

  expect_identical(as.character(censored$status), "0")
  expect_equal(censored$time, 10)
})

test_that("tie handling is validated and recorded in result settings", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  expect_error(
    addCohortSurvival(
      cdm$mgus_diagnosis,
      cdm = cdm,
      outcomeCohortTable = "death_cohort",
      censorTie = "unknown"
    )
  )
  expect_error(
    estimateCompetingRiskSurvival(
      cdm,
      targetCohortTable = "mgus_diagnosis",
      outcomeCohortTable = "progression",
      competingOutcomeCohortTable = "death_cohort",
      outcomeTie = "unknown"
    )
  )

  result <- estimateCompetingRiskSurvival(
    cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "progression",
    competingOutcomeCohortTable = "death_cohort",
    outcomeTie = "competingOutcome",
    censorTie = "censor"
  )
  settings <- omopgenerics::settings(result)

  expect_true(all(settings$outcome_competing_tie == "competingOutcome"))
  expect_true(all(settings$event_censor_tie == "censor"))

  CDMConnector::cdmDisconnect(cdm)
})
