# Copyright 2023 DARWIN EU (C)
#
# This file is part of CohortSurvival
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' A tidy implementation of the summarised_characteristics object.
#'
#' @param result A summarised_characteristics object.
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(
#'   cdm = cdm,
#'   targetCohortTable = "mgus_diagnosis",
#'   targetCohortId = 1,
#'   outcomeCohortTable = "death_cohort",
#'   outcomeCohortId = 1,
#'   eventGap = 7
#' ) %>%
#'   asSurvivalResult()
#' }
#'
#' @return A tibble with a tidy version of the summarised_characteristics
#' object.
#'
#' @export
#'
asSurvivalResult <- function(result) {
  result <- omopgenerics::newSummarisedResult(result)
  if (!inherits(result, "summarised_result")) {
    cli::cli_abort("result is not a valid `summarised_result` object.")
  }
  result <- result %>%
    visOmopResults::addSettings() %>%
    #    suppress(minCellCount = minCellCount) %>%
    dplyr::select(-c("package_name", "package_version", "estimate_type")) %>%
    visOmopResults::splitAdditional() %>%
    visOmopResults::splitGroup() %>%
    dplyr::mutate(estimate_value = as.numeric(.data$estimate_value))
  estimates <- result %>%
    dplyr::filter(.data$variable_name %in%
                    c("survival_probability",
                      "cumulative_failure_probability")) %>%
    dplyr::select(-dplyr::any_of('eventgap')) %>%
    dplyr::mutate(time = as.numeric(.data$time))
  if("competing_outcome" %in% colnames(estimates)) {
    estimates <- estimates %>%
      dplyr::relocate("outcome", .after = "cohort") %>%
      dplyr::relocate("competing_outcome", .after = "outcome")
  } else {
    estimates <- estimates %>%
      dplyr::relocate("outcome", .after = "cohort")
  }
  summary <- result %>%
    dplyr::filter(.data$variable_name == 'survival_summary') %>%
    dplyr::select(-dplyr::any_of(c('variable_name', 'time', 'eventgap')))
  events <- result %>%
    dplyr::filter(.data$variable_name == 'survival_events') %>%
    dplyr::select(-dplyr::any_of('variable_name')) %>%
    dplyr::distinct() %>%
    dplyr::mutate(time = as.numeric(.data$time))

  result_final <- estimates
  attr(result_final, 'events') <- events
  attr(result_final, 'summary') <- summary

  return(result_final)
}
