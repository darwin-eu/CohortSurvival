test_that("addCohortSurvival adds time and status for multiple outcomes", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  cdm <- bind(cdm$progression, cdm$death_cohort, name = "outcome_cohorts")

  multiple <- addCohortSurvival(
    cdm$mgus_diagnosis,
    cdm = cdm,
    outcomeCohortTable = "outcome_cohorts",
    outcomeCohortId = c("progression", "death_cohort"),
    outcomeWashout = 0
  )
  progression <- addCohortSurvival(
    cdm$mgus_diagnosis,
    cdm = cdm,
    outcomeCohortTable = "outcome_cohorts",
    outcomeCohortId = "progression",
    outcomeWashout = 0
  )
  death <- addCohortSurvival(
    cdm$mgus_diagnosis,
    cdm = cdm,
    outcomeCohortTable = "outcome_cohorts",
    outcomeCohortId = "death_cohort",
    outcomeWashout = 0
  )

  multiple <- multiple |>
    dplyr::arrange(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) |>
    dplyr::collect()
  progression <- progression |>
    dplyr::arrange(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) |>
    dplyr::collect()
  death <- death |>
    dplyr::arrange(.data$subject_id, .data$cohort_start_date, .data$cohort_end_date) |>
    dplyr::collect()

  expect_true(all(c(
    "progression_time", "progression_status",
    "death_cohort_time", "death_cohort_status"
  ) %in% names(multiple)))
  keys <- c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  )
  progression_check <- multiple |>
    dplyr::inner_join(
      progression |>
        dplyr::select(dplyr::all_of(keys), "time", "status"),
      by = keys
    )
  death_check <- multiple |>
    dplyr::inner_join(
      death |>
        dplyr::select(dplyr::all_of(keys), "time", "status"),
      by = keys
    )

  expect_equal(progression_check$progression_time, progression_check$time)
  expect_equal(progression_check$progression_status, progression_check$status)
  expect_equal(death_check$death_cohort_time, death_check$time)
  expect_equal(death_check$death_cohort_status, death_check$status)

  CDMConnector::cdmDisconnect(cdm)
})
