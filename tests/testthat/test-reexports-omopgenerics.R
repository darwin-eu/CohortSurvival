
test_that("omopgenerics reexports work", {
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      "mgus_diagnosis",
                                      "death_cohort") |>
    omopgenerics::suppress(minCellCount = 5)

  survCR <- estimateCompetingRiskSurvival(cdm,
                                          "mgus_diagnosis",
                                          "progression",
                                          "death_cohort")

  # importing and exporting
  result_path <- tempdir()
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
                                           "death_cohort") |>
    omopgenerics::suppress(minCellCount = 0)

  expect_false(isTRUE(all.equal(surv, surv_nosup, check.attributes = FALSE)))

  omopgenerics::exportSummarisedResult(surv_nosup, path = result_path)
  surv_nosup_imported <-  omopgenerics::importSummarisedResult(result_path)
  expect_true(isTRUE(all.equal(surv, surv_nosup_imported, check.attributes = FALSE)))

  CDMConnector::cdmDisconnect(cdm)

})

test_that("omopgenerics filterSettings", {
  cdmSurvival <- mockMGUS2cdm()
  single_event <- estimateSingleEventSurvival(cdmSurvival,
                                              targetCohortTable = "mgus_diagnosis",
                                              targetCohortId = 1,
                                              outcomeCohortTable = "death_cohort",
                                              outcomeCohortId = 1,
                                              strata = list(c("age_group"),
                                                            c("sex"),
                                                            c("age_group", "sex")))

  competing_risk <- estimateCompetingRiskSurvival(cdmSurvival,
                                                  targetCohortTable = "mgus_diagnosis",
                                                  outcomeCohortTable = "progression",
                                                  competingOutcomeCohortTable = "death_cohort",
                                                  strata = list(c("sex")))

  study_result <- omopgenerics::bind(single_event, competing_risk)

  expect_true(all(study_result |>
                omopgenerics::filterSettings(analysis_type == "competing_risk") |>
                dplyr::select(result_id) |> dplyr::arrange(result_id) |>
                dplyr::distinct() == c(5,6,7,8)))

  expect_true(all(study_result |>
                    omopgenerics::filterSettings(analysis_type == "single_event") |>
                    dplyr::select(result_id) |> dplyr::arrange(result_id) |>
                    dplyr::distinct() == c(1,2,3,4)))

  expect_warning(study_result |>
                    omopgenerics::tidy())

  expect_no_error(study_result |>
                    omopgenerics::filterSettings(result_type == "survival_events") |>
                    omopgenerics::tidy())

  CDMConnector::cdmDisconnect(cdmSurvival)
})
