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

  required_cols <- c("result_id","cdm_name", "group_name", "group_level", "strata_name",
                     "strata_level", "variable_name", "variable_level",
                     "estimate_name", "estimate_type", "estimate_value",
                     "additional_name", "additional_level", "result_type",
                     "outcome", "competing_outcome", "eventgap")

  result <- result %>%
     omopgenerics::addSettings(
       settingsColumn = c("result_type",omopgenerics::settingsColumns(result)))

  non_constant_cols <- result %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.))) %>%
    purrr::keep(~ .x > 1) %>%
    names()

  if (length(non_constant_cols[!(non_constant_cols %in% required_cols)]) > 0) {
    cli::cli_warn("{.strong {non_constant_cols[!(non_constant_cols %in% required_cols)]}} column{?s} will be added to the survival result object to include all relevant information")
  }

  final_cols <- dplyr::union(required_cols, non_constant_cols)

  result <- result %>%
    dplyr::select(dplyr::all_of(final_cols)) %>%
    omopgenerics::splitAdditional() %>%
    omopgenerics::splitGroup() %>%
    omopgenerics::splitStrata()

  estimates <- result %>%
    dplyr::filter(.data$result_type %in%
                    c("survival_probability",
                      "cumulative_failure_probability")) %>%
    dplyr::select(-dplyr::any_of(c("eventgap", "reason_id", "reason", "result_id"))) %>%
    dplyr::mutate(time = as.numeric(.data$time)) %>%
      dplyr::relocate("outcome", .after = "target_cohort") %>%
      dplyr::relocate("competing_outcome", .after = "outcome") %>%
    omopgenerics::pivotEstimates() %>%
    dplyr::select(-dplyr::starts_with("variable_name")) %>%
    dplyr::rename("variable" = "variable_level")

  summary <- result %>%
    dplyr::filter(.data$result_type == "survival_summary") %>%
    dplyr::select(-dplyr::any_of(c("variable_name", "time", "eventgap", "result_type", "reason_id", "result_id", "reason"))) %>%
    dplyr::mutate(estimate_name = dplyr::if_else(
      grepl("count", .data$estimate_name),
      gsub("_count","",.data$estimate_name),
      .data$estimate_name
    )) %>%
    dplyr::select(-dplyr::starts_with("variable_name")) %>%
    dplyr::rename("variable" = "variable_level") %>%
    dplyr::relocate("outcome", .after = "target_cohort") %>%
    dplyr::relocate("competing_outcome", .after = "outcome") %>%
    omopgenerics::pivotEstimates()

  events <- result %>%
    dplyr::filter(.data$result_type == "survival_events") %>%
    dplyr::select(-dplyr::any_of(c("reason_id", "result_type", "result_id", "reason"))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(time = as.numeric(.data$time)) %>%
    dplyr::mutate(estimate_name = dplyr::if_else(
      grepl("count", .data$estimate_name),
      gsub("_count","",.data$estimate_name),
      .data$estimate_name
    )) %>%
    dplyr::select(-dplyr::starts_with("variable_name")) %>%
    dplyr::rename("variable" = "variable_level") %>%
    dplyr::relocate("outcome", .after = "target_cohort") %>%
    dplyr::relocate("competing_outcome", .after = "outcome") %>%
    omopgenerics::pivotEstimates()

  attrition <- result %>%
    dplyr::filter(.data$result_type == "survival_attrition") %>%
    dplyr::select(-c("result_type", "time", "eventgap", "result_id", "reason_id")) %>%
    dplyr::select(-dplyr::starts_with("variable_level")) %>%
    dplyr::relocate("outcome", .after = "target_cohort") %>%
    dplyr::relocate("competing_outcome", .after = "outcome") %>%
    dplyr::distinct() %>%
    omopgenerics::pivotEstimates()

  result_final <- estimates
  attr(result_final, "events") <- events
  attr(result_final, "summary") <- summary
  attr(result_final, "attrition") <- attrition

  return(result_final)
}
