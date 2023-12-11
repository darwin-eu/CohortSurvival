test_that("mgus example: benchmark", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  cdm$condition_occurrence <- cdm$death_cohort %>%
    dplyr::rename("condition_start_date" = "cohort_start_date",
                  "condition_end_date" = "cohort_end_date")
  cdm$drug_exposure <- cdm$progression %>%
    dplyr::rename("drug_exposure_start_date" = "cohort_start_date",
                  "drug_exposure_end_date" = "cohort_end_date")
  timings <- benchmarkCohortSurvival(cdm, targetSize = 1000, outcomeSize = 47)
  timings_p <- benchmarkCohortSurvival(cdm, targetSize = 1000, outcomeSize = 47, returnParticipants = TRUE)
  timings_s <- benchmarkCohortSurvival(cdm, targetSize = 100, outcomeSize = 5, strata = list("sex" = c("sex")))
  timings2 <- benchmarkCohortSurvival(cdm, targetSize = 1000, outcomeSize = 8)
  timings3 <- benchmarkCohortSurvival(cdm, targetSize = 100, outcomeSize = 23)
  timings4 <- benchmarkCohortSurvival(cdm, targetSize = 100, outcomeSize = 23, outcomeDateVariable = "cohort_end_date")
  timings5 <- benchmarkCohortSurvival(cdm, targetSize = 1000, competingOutcomeSize = 80, outcomeSize = 90, outcomeDateVariable = "cohort_end_date")
  timings6 <- benchmarkCohortSurvival(cdm, targetSize = 100, outcomeSize = 7, censorOnCohortExit = TRUE)
  timings7 <- benchmarkCohortSurvival(cdm, targetSize = 100, outcomeSize = 4, censorOnDate = as.Date("1920-01-01"))
  timings8 <- benchmarkCohortSurvival(cdm, targetSize = 1000, outcomeSize = 47, followUpDays = 30)
  timings9 <- benchmarkCohortSurvival(cdm, targetSize = 1000, outcomeSize = 47, minCellCount = 2)

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings)))
  expect_true(all(c("generating target cohort size 1000", "generating outcome cohort size 47") %in% timings$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings_p)))
  expect_true(all(c("generating target cohort size 1000", "generating outcome cohort size 47") %in% timings_p$task))
  expect_true(timings_p %>% dplyr::select(with_participants) %>% dplyr::distinct() %>% dplyr::pull() == "Yes")

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings_s)))
  expect_true(all(c("generating target cohort size 100", "generating outcome cohort size 5") %in% timings_s$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings2)))
  expect_true(all(c("generating target cohort size 1000", "generating outcome cohort size 8") %in% timings2$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings3)))
  expect_true(all(c("generating target cohort size 100", "generating outcome cohort size 23") %in% timings3$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings4)))
  expect_true(all(c("generating target cohort size 100", "generating outcome cohort size 23") %in% timings4$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings5)))
  expect_true(all(c("generating target cohort size 1000", "generating outcome cohort size 90", "generating competing outcome cohort size 80") %in% timings5$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings6)))
  expect_true(all(c("generating target cohort size 100", "generating outcome cohort size 7") %in% timings6$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings7)))
  expect_true(all(c("generating target cohort size 100", "generating outcome cohort size 4") %in% timings7$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings8)))
  expect_true(all(c("generating target cohort size 1000", "generating outcome cohort size 47") %in% timings8$task))

  expect_true(all(c("time_taken_secs", "time_taken_mins", "time_taken_hours", "dbms", "person_n", "db_min_observation_start", "max_observation_end","with_participants") %in% colnames(timings9)))
  expect_true(all(c("generating target cohort size 1000", "generating outcome cohort size 47") %in% timings9$task))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("expected errors benchmark", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  cdm$condition_occurrence <- cdm$death_cohort %>%
    dplyr::rename("condition_start_date" = "cohort_start_date",
                  "condition_end_date" = "cohort_end_date")
  expect_error(benchmarkCohortSurvival("cdm"))
  expect_error(benchmarkCohortSurvival(cdm))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = "size"))
  expect_error(benchmarkCohortSurvival(targetSize = 30))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = "3"))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, competingOutcomeSize = "no"))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, outcomeDateVariable = FALSE))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, censorOnCohortExit = NULL))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, censorOnDate = TRUE))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, followUpDays = "Inf"))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, strata = c("age" = "age")))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, timeGap = list(1,2)))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, times = list(1,2,3)))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, minCellCount = FALSE))
  expect_error(benchmarkCohortSurvival(cdm, targetSize = 100, outcomSize = 40, returnParticipants = "TRUE"))

  CDMConnector::cdmDisconnect(cdm)
})
