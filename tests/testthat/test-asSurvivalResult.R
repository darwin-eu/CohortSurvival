test_that("validateSurvivalResult works correctly", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortTable = "death_cohort")

  expect_no_error(validateSurvivalResult(surv))
  expect_no_error(validateSurvivalResult(surv, required_types = "survival_summary"))
  expect_no_error(validateSurvivalResult(surv, required_types = c("survival_estimates")))
  expect_error(validateSurvivalResult(surv, required_types = "cumulative_failure_probability"))
  expect_error(validateSurvivalResult(data.frame(x = 1)))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("asSurvivalResult works with filtered results", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortTable = "death_cohort")

  prob_only <- surv |>
    omopgenerics::filterSettings(result_type == "survival_estimates")

  expect_no_error(result <- asSurvivalResult(prob_only))
  expect_true(tibble::is_tibble(result))

  summary_only <- surv |>
    omopgenerics::filterSettings(result_type == "survival_summary")

  expect_no_error(result <- asSurvivalResult(summary_only))
  expect_true(tibble::is_tibble(result))

  events_only <- surv |>
    omopgenerics::filterSettings(result_type == "survival_events")

  expect_no_error(result <- asSurvivalResult(events_only))
  expect_true(tibble::is_tibble(result))

  attrition_only <- surv |>
    omopgenerics::filterSettings(result_type == "survival_attrition")

  expect_no_error(result <- asSurvivalResult(attrition_only))
  expect_true(tibble::is_tibble(result))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("plotSurvival works with filtered results", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm,
                                      targetCohortTable = "mgus_diagnosis",
                                      outcomeCohortTable = "death_cohort")

  prob_only <- surv |>
    omopgenerics::filterSettings(result_type == "survival_estimates")

  expect_no_error(plot <- plotSurvival(prob_only))
  expect_true(ggplot2::is.ggplot(plot))

  summary_only <- surv |>
    omopgenerics::filterSettings(result_type == "survival_summary")

  expect_error(plotSurvival(summary_only))

  survCR <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "mgus_diagnosis",
                                          outcomeCohortTable = "progression",
                                          competingOutcomeCohortTable = "death_cohort")

  cumulative_only <- survCR |>
    omopgenerics::filterSettings(analysis_type == "competing_risk")

  expect_no_error(plot <- plotSurvival(cumulative_only, cumulativeFailure = TRUE, colour = "variable"))
  expect_true(ggplot2::is.ggplot(plot))

  surv_all <- omopgenerics::bind(surv, survCR)
  expect_no_error(plot <- plotSurvival(surv_all, cumulativeFailure = TRUE, facet = "analysis_type", colour = "variable"))

  surv_all <- surv_all |>
    omopgenerics::filterSettings(result_type %in% c("survival_estimates"))
  expect_no_error(plot <- plotSurvival(surv_all, cumulativeFailure = TRUE))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("Round-trip conversion preserves data exactly - Single Event", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()

  # Test 1: Single event without strata
  surv_original <- estimateSingleEventSurvival(cdm,
                                              targetCohortTable = "mgus_diagnosis",
                                              outcomeCohortTable = "death_cohort")

  # Convert to survival result and back
  surv_converted <- asSurvivalResult(surv_original)
  surv_back <- asSummarisedResult(surv_converted)

  # Check exact equality using anti_join
  diff_original_to_back <- surv_original |> dplyr::anti_join(surv_back, by = names(surv_original))
  diff_back_to_original <- surv_back |> dplyr::anti_join(surv_original, by = names(surv_back))

  expect_equal(nrow(diff_original_to_back), 0)
  expect_equal(nrow(diff_back_to_original), 0)

  # Test 2: Single event with individual strata
  surv_strata_single <- estimateSingleEventSurvival(cdm,
                                                   targetCohortTable = "mgus_diagnosis",
                                                   outcomeCohortTable = "death_cohort",
                                                   strata = list("sex" = "sex"))

  surv_strata_converted <- asSurvivalResult(surv_strata_single)
  surv_strata_back <- asSummarisedResult(surv_strata_converted)

  diff_strata_orig_back <- surv_strata_single |> dplyr::anti_join(surv_strata_back, by = names(surv_strata_single))
  diff_strata_back_orig <- surv_strata_back |> dplyr::anti_join(surv_strata_single, by = names(surv_strata_back))

  expect_equal(nrow(diff_strata_orig_back), 0)
  expect_equal(nrow(diff_strata_back_orig), 0)

  # Test 3: Single event with multiple simultaneous strata
  surv_multi_strata <- estimateSingleEventSurvival(cdm,
                                                  targetCohortTable = "mgus_diagnosis",
                                                  outcomeCohortTable = "death_cohort",
                                                  strata = list("sex_and_age" = c("sex", "age_group")))

  surv_multi_converted <- asSurvivalResult(surv_multi_strata)
  surv_multi_back <- asSummarisedResult(surv_multi_converted)

  diff_multi_orig_back <- surv_multi_strata |> dplyr::anti_join(surv_multi_back, by = names(surv_multi_strata))
  diff_multi_back_orig <- surv_multi_back |> dplyr::anti_join(surv_multi_strata, by = names(surv_multi_back))

  expect_equal(nrow(diff_multi_orig_back), 0)
  expect_equal(nrow(diff_multi_back_orig), 0)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("Round-trip conversion preserves data exactly - Competing Risk", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()

  # Test 1: Competing risk without strata
  surv_cr_original <- estimateCompetingRiskSurvival(cdm,
                                                   targetCohortTable = "mgus_diagnosis",
                                                   outcomeCohortTable = "progression",
                                                   competingOutcomeCohortTable = "death_cohort")

  surv_cr_converted <- asSurvivalResult(surv_cr_original)
  surv_cr_back <- asSummarisedResult(surv_cr_converted)

  diff_cr_orig_back <- surv_cr_original |> dplyr::anti_join(surv_cr_back, by = names(surv_cr_original))
  diff_cr_back_orig <- surv_cr_back |> dplyr::anti_join(surv_cr_original, by = names(surv_cr_back))

  expect_equal(nrow(diff_cr_orig_back), 0)
  expect_equal(nrow(diff_cr_back_orig), 0)

  # Test 2: Competing risk with individual strata
  surv_cr_strata <- estimateCompetingRiskSurvival(cdm,
                                                 targetCohortTable = "mgus_diagnosis",
                                                 outcomeCohortTable = "progression",
                                                 competingOutcomeCohortTable = "death_cohort",
                                                 strata = list("sex" = "sex"))

  surv_cr_strata_converted <- asSurvivalResult(surv_cr_strata)
  surv_cr_strata_back <- asSummarisedResult(surv_cr_strata_converted)

  diff_cr_strata_orig <- surv_cr_strata |> dplyr::anti_join(surv_cr_strata_back, by = names(surv_cr_strata))
  diff_cr_strata_back <- surv_cr_strata_back |> dplyr::anti_join(surv_cr_strata, by = names(surv_cr_strata_back))

  expect_equal(nrow(diff_cr_strata_orig), 0)
  expect_equal(nrow(diff_cr_strata_back), 0)

  # Test 3: Competing risk with multiple simultaneous strata
  surv_cr_multi <- estimateCompetingRiskSurvival(cdm,
                                                targetCohortTable = "mgus_diagnosis",
                                                outcomeCohortTable = "progression",
                                                competingOutcomeCohortTable = "death_cohort",
                                                strata = list("sex_and_age" = c("sex", "age_group")))

  surv_cr_multi_converted <- asSurvivalResult(surv_cr_multi)
  surv_cr_multi_back <- asSummarisedResult(surv_cr_multi_converted)

  diff_cr_multi_orig <- surv_cr_multi |> dplyr::anti_join(surv_cr_multi_back, by = names(surv_cr_multi))
  diff_cr_multi_back <- surv_cr_multi_back |> dplyr::anti_join(surv_cr_multi, by = names(surv_cr_multi_back))

  expect_equal(nrow(diff_cr_multi_orig), 0)
  expect_equal(nrow(diff_cr_multi_back), 0)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("tableSurvival produces identical results with both formats", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()

  # Test with single event
  surv_original <- estimateSingleEventSurvival(cdm,
                                              targetCohortTable = "mgus_diagnosis",
                                              outcomeCohortTable = "death_cohort")
  surv_converted <- asSurvivalResult(surv_original)

  table_original <- tableSurvival(surv_original, type = "tibble")
  table_converted <- tableSurvival(surv_converted, type = "tibble")

  expect_true(identical(table_original, table_converted))

  # Test with competing risk
  surv_cr <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "mgus_diagnosis",
                                          outcomeCohortTable = "progression",
                                          competingOutcomeCohortTable = "death_cohort")
  surv_cr_converted <- asSurvivalResult(surv_cr)

  table_cr_original <- tableSurvival(surv_cr, type = "tibble")
  table_cr_converted <- tableSurvival(surv_cr_converted, type = "tibble")

  expect_true(identical(table_cr_original, table_cr_converted))

  # Test with strata
  surv_strata <- estimateSingleEventSurvival(cdm,
                                            targetCohortTable = "mgus_diagnosis",
                                            outcomeCohortTable = "death_cohort",
                                            strata = list("sex" = "sex"))
  surv_strata_converted <- asSurvivalResult(surv_strata)

  table_strata_original <- tableSurvival(surv_strata, type = "tibble")
  table_strata_converted <- tableSurvival(surv_strata_converted, type = "tibble")

  expect_true(identical(table_strata_original, table_strata_converted))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("plotSurvival works identically with both formats", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()

  # Test with single event (we can't compare plots directly, but check they don't error)
  surv_original <- estimateSingleEventSurvival(cdm,
                                              targetCohortTable = "mgus_diagnosis",
                                              outcomeCohortTable = "death_cohort")
  surv_converted <- asSurvivalResult(surv_original)

  expect_no_error(plot_original <- plotSurvival(surv_original))
  expect_no_error(plot_converted <- plotSurvival(surv_converted))
  expect_true(ggplot2::is.ggplot(plot_original))
  expect_true(ggplot2::is.ggplot(plot_converted))

  # Test with competing risk
  surv_cr <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "mgus_diagnosis",
                                          outcomeCohortTable = "progression",
                                          competingOutcomeCohortTable = "death_cohort")
  surv_cr_converted <- asSurvivalResult(surv_cr)

  expect_no_error(plot_cr_original <- plotSurvival(surv_cr, cumulativeFailure = TRUE))
  expect_no_error(plot_cr_converted <- plotSurvival(surv_cr_converted, cumulativeFailure = TRUE))
  expect_true(ggplot2::is.ggplot(plot_cr_original))
  expect_true(ggplot2::is.ggplot(plot_cr_converted))

  # Test with strata
  surv_strata <- estimateSingleEventSurvival(cdm,
                                            targetCohortTable = "mgus_diagnosis",
                                            outcomeCohortTable = "death_cohort",
                                            strata = list("sex" = "sex"))
  surv_strata_converted <- asSurvivalResult(surv_strata)

  expect_no_error(plot_strata_original <- plotSurvival(surv_strata, colour = "sex"))
  expect_no_error(plot_strata_converted <- plotSurvival(surv_strata_converted, colour = "sex"))
  expect_true(ggplot2::is.ggplot(plot_strata_original))
  expect_true(ggplot2::is.ggplot(plot_strata_converted))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("riskTable produces identical results where applicable", {
  skip_on_cran()

  cdm <- mockMGUS2cdm()

  # riskTable only works with summarised_result format, so test that conversion preserves the capability
  surv_original <- estimateSingleEventSurvival(cdm,
                                              targetCohortTable = "mgus_diagnosis",
                                              outcomeCohortTable = "death_cohort")
  surv_converted <- asSurvivalResult(surv_original)
  surv_back <- asSummarisedResult(surv_converted)

  expect_no_error(risk_table_original <- riskTable(surv_original, type = "tibble"))
  expect_no_error(risk_table_back <- riskTable(surv_back, type = "tibble"))
  expect_true(tibble::is_tibble(risk_table_original))
  expect_true(tibble::is_tibble(risk_table_back))

  # Also feasible with survival type
  expect_no_error(riskTable(surv_converted, type = "tibble"))
  # But needs to have events
  expect_error(riskTable(surv_original |>
                           omopgenerics::filterSettings(result_type == "survival_summary"), type = "tibble"))

  # The tables should be identical after round-trip
  expect_true(identical(risk_table_original, risk_table_back))

  # Test with competing risk
  surv_cr <- estimateCompetingRiskSurvival(cdm,
                                          targetCohortTable = "mgus_diagnosis",
                                          outcomeCohortTable = "progression",
                                          competingOutcomeCohortTable = "death_cohort")
  surv_cr_converted <- asSurvivalResult(surv_cr)
  surv_cr_back <- asSummarisedResult(surv_cr_converted)

  expect_no_error(risk_cr_original <- riskTable(surv_cr, type = "tibble"))
  expect_no_error(risk_cr_back <- riskTable(surv_cr_back, type = "tibble"))
  expect_true(identical(risk_cr_original, risk_cr_back))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("asSurvivalResult and asSummarisedResult expected errors",{
  skip_on_cran()
  cdm <- mockMGUS2cdm()
  surv <- estimateSingleEventSurvival(cdm, "mgus_diagnosis", "death_cohort")
  surv_sr <- surv |>
    asSurvivalResult()
  expect_warning(asSurvivalResult(omopgenerics::emptySummarisedResult()))
  expect_error(validateSurvivalResult(omopgenerics::emptySummarisedResult(), required_types = "survival_estimates"))
  expect_error(asSummarisedResult(surv))
  attr(surv_sr, "settings") <- NULL
  expect_error(asSummarisedResult(surv_sr))
  CDMConnector::cdmDisconnect(cdm)
})
