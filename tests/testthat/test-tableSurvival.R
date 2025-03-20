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
  res <- tableSurvival(surv, times = c(100,200), type = "tibble")
  expect_true(res %>%
              dplyr::tally() == 1)
   expect_true(all(
     colnames(res) ==
       c('CDM name', 'Target cohort', 'Outcome name',
         '[header_name]Estimate name\n[header_level]Number records',
         '[header_name]Estimate name\n[header_level]Number events',
         '[header_name]Estimate name\n[header_level]Median survival (95% CI)',
         '[header_name]Estimate name\n[header_level]Restricted mean survival (SE)',
         '[header_name]Estimate name\n[header_level]100 days survival estimate',
         '[header_name]Estimate name\n[header_level]200 days survival estimate')))

  survCR <- estimateCompetingRiskSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "progression",
                                      outcomeCohortId = 1,
                                      competingOutcomeCohortTable = "death_cohort",
                                      eventGap = 7
  )

  gt1 <- tableSurvival(survCR, times = c(100,200))
  expect_true(gt1$`_data` %>% dplyr::tally() == 2)
   expect_true(all(
     colnames(gt1$`_data`) ==
       c('CDM name', 'Target cohort', 'Outcome type', 'Outcome name',
         '[header_name]Estimate name\n[header_level]Number records',
         '[header_name]Estimate name\n[header_level]Number events',
         '[header_name]Estimate name\n[header_level]Restricted mean survival',
         '[header_name]Estimate name\n[header_level]100 days survival estimate',
         '[header_name]Estimate name\n[header_level]200 days survival estimate')))

  fx1 <- tableSurvival(survCR, type = "flextable")
  expect_true(fx1$body$dataset %>% dplyr::tally() == 2)
   expect_true(all(
     colnames(fx1$body$dataset ) ==
       c('CDM name', 'Target cohort', 'Outcome type', 'Outcome name',
         'Estimate name\nNumber records', 'Estimate name\nNumber events',
         'Estimate name\nRestricted mean survival')))

  survsex <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      strata = list("sex" = "sex"),
                                      eventGap = 7
  )

  gt2 <- tableSurvival(survsex)
   expect_true(all(
     colnames(gt2$`_data`) ==
       c('CDM name', 'Target cohort', 'Sex', 'Outcome name',
         '[header_name]Estimate name\n[header_level]Number records',
         '[header_name]Estimate name\n[header_level]Number events',
         '[header_name]Estimate name\n[header_level]Median survival (95% CI)',
         '[header_name]Estimate name\n[header_level]Restricted mean survival (SE)')))

  gt3 <- tableSurvival(survsex, header = c("cdm_name", "group"))
   expect_true(all(
    colnames(gt3$`_data`) ==
       c('Sex', 'Outcome name', 'Estimate name',
         '[header_name]CDM name\n[header_level]mock\n[header_name]Target cohort\n[header_level]mgus_diagnosis')))

  # In years
  expect_true(all(tableSurvival(surv, times = c(365,420), type = "tibble") %>%
                    dplyr::select(-dplyr::contains("mean"), -dplyr::contains("median")) ==
                    tableSurvival(surv, times = c(1,1.15), timeScale = "years", type = "tibble") %>%
                    dplyr::select(-dplyr::contains("mean"), -dplyr::contains("median")) ))

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
  expect_error(tableSurvival(surv, times = "a"))
  expect_error(tableSurvival(surv, times = c(1,2,3), timeScale = "day"))

  CDMConnector::cdmDisconnect(cdm)

})
