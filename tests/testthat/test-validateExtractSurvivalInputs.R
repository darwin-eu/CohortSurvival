test_that("validateExtractSurvivalInputs validates outcome cohort id once", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  expect_silent(
    CohortSurvival:::validateExtractSurvivalInputs(
      cdm = cdm,
      cohortTable = cdm[["mgus_diagnosis"]],
      outcomeCohortTable = "death_cohort",
      outcomeCohortId = 1,
      outcomeWashout = 0,
      censorOnCohortExit = FALSE,
      censorOnDate = NULL,
      followUpDays = Inf
    )
  )

  expect_error(
    CohortSurvival:::validateExtractSurvivalInputs(
      cdm = cdm,
      cohortTable = cdm[["mgus_diagnosis"]],
      outcomeCohortTable = "death_cohort",
      outcomeCohortId = 999,
      outcomeWashout = 0,
      censorOnCohortExit = FALSE,
      censorOnDate = NULL,
      followUpDays = Inf
    )
  )

  CDMConnector::cdmDisconnect(cdm)
})
