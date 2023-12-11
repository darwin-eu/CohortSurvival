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

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("plot years on x axis", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                           targetCohortTable = "mgus_diagnosis",
                           targetCohortId = 1,
                           outcomeCohortTable = "death_cohort",
                           outcomeCohortId = 1
  )

  plot <- plotSurvival(surv, xscale = c("years"))
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("plot facets", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
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

test_that("plot facets - multiple column", {
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
                      facet = c("cdm_name","strata_level"))
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("plot colour", {
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
                       facet = "strata_level",
                       colour = "strata_level")

  expect_true(ggplot2::is.ggplot(plot))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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

  plot <- plotCumulativeIncidence(surv, xscale = "years")
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("plot facets for cumulative incidence plots", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
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
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
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

