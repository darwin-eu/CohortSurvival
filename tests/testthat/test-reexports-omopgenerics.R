
test_that("omopgenerics reexports work", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      "mgus_diagnosis",
                                      "death_cohort") %>%
    omopgenerics::suppress(minCellCount = 5)

  survCR <- estimateCompetingRiskSurvival(cdm,
                                          "mgus_diagnosis",
                                          "progression",
                                          "death_cohort")

  # importing and exporting
  result_path <- tempdir("result")
  expect_warning(omopgenerics::exportSummarisedResult(surv, path = result_path))
  surv_imported <-  omopgenerics::importSummarisedResult(result_path)
  expect_no_error(tableSurvival(surv_imported, type = "tibble"))
  expect_no_error(dplyr::is.tbl(omopgenerics::settings(surv_imported)))

  # result type using bind
  expect_no_error(omopgenerics::validateResultArgument(surv_imported))
  expect_no_error(omopgenerics::validateResultArgument(omopgenerics::bind(surv, survCR)))

  # suppresing results
  surv_nosup <- estimateSingleEventSurvival(cdm,
                                           "mgus_diagnosis",
                                           "death_cohort") %>%
    omopgenerics::suppress(minCellCount = 0)

  expect_false(isTRUE(all.equal(surv, surv_nosup, check.attributes = FALSE)))

  omopgenerics::exportSummarisedResult(surv_nosup, path = result_path)
  surv_nosup_imported <-  omopgenerics::importSummarisedResult(result_path)
  expect_true(isTRUE(all.equal(surv, surv_nosup_imported, check.attributes = FALSE)))

  CDMConnector::cdmDisconnect(cdm)

})
