test_that("basic Survival plot", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1
  )

  plot <- plotSurvival(surv)
  expect_true(ggplot2::is.ggplot(plot))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("plot facets and colour", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1,
                           strata=list(c("sex", "age_group"))
  )

  plot <- plotSurvival(surv,
                      facet = "sex",
                      colour = "age_group")
  expect_true(ggplot2::is.ggplot(plot))

  plot <- plotSurvival(surv,
                       facet = c("sex","age_group"),
                       colour = "cdm_name")
  expect_true(ggplot2::is.ggplot(plot))

  plot <- plotSurvival(surv,
                       facet = "sex",
                       colour = "age_group")
  expect_true(ggplot2::is.ggplot(plot))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("basic cumulative incidence plot", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1
  )

  plot <- plotSurvival(surv, cumulativeFailure = TRUE)
  expect_true(ggplot2::is.ggplot(plot))

  survCR <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "mgus_diagnosis",
                                          outcomeCohortTable = "progression",
                                          competingOutcomeCohortTable = "death_cohort"
  )
  plot <- plotSurvival(survCR, cumulativeFailure = TRUE,
                       colour = "variable")
  expect_true(ggplot2::is.ggplot(plot))

  # cumulativeFailure must be true when working with competing risk result
  expect_error(plotSurvival(survCR, xscale = "years", cumulativeFailure = FALSE,
                       colour = "variable"))

  CDMConnector::cdmDisconnect(cdm)

})

test_that("plot risk tables", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1,
                                      strata=list(c("sex", "age_group"))
  )
  plot <- plotSurvival(surv,
                       facet = "sex",
                       colour = "age_group",
                       riskTable = TRUE)

  expect_true(ggplot2::is.ggplot(plot))

  plot <- plotSurvival(surv,
                       colour = c("age_group", "sex"),
                       riskTable = TRUE)

  expect_true(ggplot2::is.ggplot(plot))

  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      targetCohortId = 1,
                                      outcomeCohortTable = "death_cohort",
                                      outcomeCohortId = 1)
  plot <- plotSurvival(surv,
                       riskTable = TRUE,
                       riskInterval = 10)

  expect_true(ggplot2::is.ggplot(plot))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("empty plot", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  surv <- omopgenerics::emptySummarisedResult()
  plotSurvival(surv)
  CDMConnector::cdmDisconnect(cdm)
})

