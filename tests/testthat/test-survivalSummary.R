test_that("survival summary", {
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      timeGap = 7
  )

  tibble::is_tibble(survivalSummary(surv))

  CDMConnector::cdmDisconnect(cdm)

  })
