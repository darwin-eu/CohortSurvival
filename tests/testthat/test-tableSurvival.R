test_that("survival summary", {
  skip_on_cran()

    cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      eventGap = 7
  )

  tibble::is_tibble(tableSurvival(surv, times = c(100,200)))
  expect_true(tableSurvival(surv, times = c(100,200)) %>%
                dplyr::tally() == 1)

  # allow no times to be provided
  tibble::is_tibble(tableSurvival(surv))

  survCR <- estimateCompetingRiskSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "progression",
                                      outcomeCohortId = 1,
                                      competingOutcomeCohortTable = "death_cohort",
                                      eventGap = 7
  )

  tibble::is_tibble(tableSurvival(survCR, times = c(100,200)))
  expect_true(tableSurvival(survCR, times = c(100,200)) %>%
                dplyr::tally() == 2)

  survsex <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      strata = list("sex" = "sex"),
                                      eventGap = 7
  )

  tibble::is_tibble(tableSurvival(survsex, times = c(100,200)))
  expect_true(tableSurvival(survsex, times = c(100,200)) %>%
                dplyr::tally() == 3)

  CDMConnector::cdmDisconnect(cdm)

  })

test_that("expected errors", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      eventGap = 7
  )

  expect_error(tableSurvival())
  expect_error(tableSurvival("surv"))

  CDMConnector::cdmDisconnect(cdm)

})
