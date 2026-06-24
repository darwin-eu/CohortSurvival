test_that("competingRiskSurvival exits when statuses not three", {
  survData <- tibble::tibble(outcome_or_competing_status = c(0, 1))
  res <- CohortSurvival:::competingRiskSurvival(survData, times = 0:1, variables = NULL, eventGap = 1, restrictedMeanFollowUp = NULL)
  expect_true(tibble::is_tibble(res))
  expect_equal(nrow(res), 0)
})
