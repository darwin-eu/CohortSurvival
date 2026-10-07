test_that("filterSurvivalChanges removes repeated probabilities", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  result <- estimateSingleEventSurvival(
    cdm,
    targetCohortTable = "mgus_diagnosis",
    outcomeCohortTable = "death_cohort",
    estimateGap = 1
  )

  compact <- filterSurvivalChanges(result)

  original_estimates <- result |>
    omopgenerics::filterSettings(result_type == "survival_estimates")
  compact_estimates <- compact |>
    omopgenerics::filterSettings(result_type == "survival_estimates")

  expect_lt(nrow(compact_estimates), nrow(original_estimates))
  expect_equal(
    omopgenerics::settings(compact),
    omopgenerics::settings(result)
  )
  expect_equal(
    compact |> omopgenerics::filterSettings(result_type != "survival_estimates"),
    result |> omopgenerics::filterSettings(result_type != "survival_estimates")
  )
  expect_no_error(plotSurvival(compact))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("filterSurvivalChanges returns results without estimates unchanged", {
  result <- omopgenerics::emptySummarisedResult()
  expect_warning(filtered <- filterSurvivalChanges(result))
  expect_identical(filtered, result)
})
