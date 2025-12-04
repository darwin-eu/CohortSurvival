test_that("test tableSurvivalAttrition", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  surv <- estimateSingleEventSurvival(
    cdm = cdm,
    targetCohortTable = "mgus_diagnosis",
    targetCohortId = 1,
    outcomeCohortTable = "death_cohort",
    outcomeCohortId = 1,
    eventGap = 7
  )

  expect_no_error(tableSurvivalAttrition(surv))
  expect_no_error(tableSurvivalAttrition(surv |>
                                           asSurvivalResult()))
  expect_error(tableSurvivalAttrition(surv |>
                                        omopgenerics::settings()))
  expect_warning(tableSurvivalAttrition(surv |>
                                           omopgenerics::filterSettings(result_type == "survival_events")))

  CDMConnector::cdmDisconnect(cdm)
})
