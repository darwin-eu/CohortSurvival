test_that("basic Survival plot", {

  cdm <- mockMGUS2cdm()
  surv <- estimateSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1
  )

  plot <- plotSurvival(surv)
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("plot facets", {

  cdm <- mockMGUS2cdm()
  surv <- estimateSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1,
                           strata=list(c("sex", "age_group"))
  )

  plot <-plotSurvival(surv,
                      facet = "strata_level")
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


test_that("plot colour", {

  cdm <- mockMGUS2cdm()
  surv <- estimateSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1,
                           strata=list(c("sex", "age_group"))
  )

  plot <- plotSurvival(surv,
                       facet = "strata_level",
                       colour = "strata_level")

  expect_true(ggplot2::is.ggplot(plot))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("basic cumulative incidence plot", {

  cdm <- mockMGUS2cdm()
  surv <- estimateSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1
  )

  plot <- plotCumulativeIncidence(surv)
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("plot facets for cumulative incidence plots", {

  cdm <- mockMGUS2cdm()
  surv <- estimateSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1,
                           strata=list(c("sex", "age_group"))
  )

  plot <-plotCumulativeIncidence(surv,
                                 facet = "strata_level")
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


test_that("plot colour for cumulative incidence plots", {

  cdm <- mockMGUS2cdm()
  surv <- estimateSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1,
                           strata=list(c("sex", "age_group"))
  )

  plot <- plotCumulativeIncidence(surv,
                                  facet = "strata_level",
                                  colour = "strata_level")

  expect_true(ggplot2::is.ggplot(plot))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

