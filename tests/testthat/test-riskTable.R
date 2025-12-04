test_that("survival events", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortTable = "death_cohort",
                                      eventGap = 7,
                                      strata = list("sex")
  )
  res <- riskTable(surv, eventGap = 7, type = "tibble")

  survCR <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "mgus_diagnosis",
                                          outcomeCohortTable = "progression",
                                          competingOutcomeCohortTable = "death_cohort"
  )
  resCR <- riskTable(survCR, type = "tibble")

  expect_true(res |>
                dplyr::tally() == 182)
  expect_true(all(
    colnames(res) ==
      c('CDM name', 'Target cohort', 'Sex', 'Outcome name', 'Time', 'Event gap',
        '[header_name]Estimate name\n[header_level]Number at risk',
        '[header_name]Estimate name\n[header_level]Number events',
        '[header_name]Estimate name\n[header_level]Number censored')))

  expect_true(resCR |>
                dplyr::tally() == 32)
  expect_true(all(
    colnames(resCR) ==
      c('CDM name', 'Target cohort', 'Outcome type', 'Outcome name', 'Time', 'Event gap',
        '[header_name]Estimate name\n[header_level]Number at risk',
        '[header_name]Estimate name\n[header_level]Number events',
        '[header_name]Estimate name\n[header_level]Number censored')))

  surv2 <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortTable = "death_cohort",
                                      eventGap = c(2,14),
                                      strata = list("sex")
  )

  res3 <- riskTable(surv2, eventGap = 14, type = "tibble")

  expect_true(all(res3 |>
    dplyr::select(Time) |>
    dplyr::pull() |>
    as.numeric() %% 14 ==
      c(rep(0,31),4,rep(0,29),2,rep(0,31),4)
  ))

  res4 <- riskTable(surv2, type = "tibble")

  expect_true(all(res4 |>
                dplyr::pull("Event gap") |>
                unique() == c("2", "14")))

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

  expect_error(riskTable())
  expect_error(riskTable("surv"))
  expect_error(riskTable(surv, eventGap = 33))
  expect_error(riskTable(surv, eventGap = 7, splitStrata = "yes"))
  expect_error(riskTable(surv, eventGap = 7, header = 2))
  expect_error(riskTable(surv, eventGap = 7, type = 2))

  CDMConnector::cdmDisconnect(cdm)

})
