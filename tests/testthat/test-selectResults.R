test_that("single-event result components can be selected", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  probability <- estimateSingleEventSurvival(
    cdm,
    "mgus_diagnosis",
    "death_cohort",
    results = "probability"
  )
  selected <- estimateSingleEventSurvival(
    cdm,
    "mgus_diagnosis",
    "death_cohort",
    results = c("summary", "attrition")
  )

  expect_identical(unique(omopgenerics::settings(probability)$result_type), "survival_estimates")
  expect_setequal(
    unique(omopgenerics::settings(selected)$result_type),
    c("survival_summary", "survival_attrition")
  )
  expect_error(
    estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort", results = "counts")
  )

  CDMConnector::cdmDisconnect(cdm)
})

test_that("competing-risk result components can be selected", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()

  selected <- estimateCompetingRiskSurvival(
    cdm,
    "mgus_diagnosis",
    "progression",
    "death_cohort",
    results = c("probability", "events")
  )

  expect_setequal(
    unique(omopgenerics::settings(selected)$result_type),
    c("survival_estimates", "survival_events")
  )

  CDMConnector::cdmDisconnect(cdm)
})
