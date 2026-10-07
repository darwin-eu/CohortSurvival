# Copyright 2023 DARWIN EU®
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

#' Keep survival estimates only when the probability changes
#'
#' Reduce the size of a survival result by removing time points at which the
#' reported survival or cumulative-incidence probability is unchanged from the
#' previous reported time. The first time point in every curve is always kept.
#' Confidence limits for a retained time point are kept with its estimate.
#'
#' Only `survival_estimates` rows are filtered. Event counts, summaries,
#' attrition, settings, and other result types are returned unchanged. This
#' makes the result suitable for `plotSurvival()`, while tables requesting an
#' exact removed time point will not be able to display that time.
#'
#' @param result A `summarised_result` produced by CohortSurvival.
#'
#' @return A `summarised_result` with unchanged probabilities removed.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' result <- estimateSingleEventSurvival(
#'   cdm,
#'   targetCohortTable = "mgus_diagnosis",
#'   outcomeCohortTable = "death_cohort"
#' )
#' compactResult <- filterSurvivalChanges(result)
#' }
filterSurvivalChanges <- function(result) {
  result <- omopgenerics::validateResultArgument(result)
  result_settings <- omopgenerics::settings(result)

  estimate_ids <- result_settings |>
    dplyr::filter(.data$result_type == "survival_estimates") |>
    dplyr::pull("result_id")

  if (length(estimate_ids) == 0) {
    cli::cli_warn("No {.val survival_estimates} results found; returning {.arg result} unchanged.")
    return(result)
  }

  estimates <- result |>
    dplyr::filter(.data$result_id %in% .env$estimate_ids)

  if (!all(estimates$additional_name == "time")) {
    cli::cli_abort("The survival estimates do not contain a {.field time} additional variable.")
  }

  curve_columns <- intersect(
    c(
      "result_id", "cdm_name", "group_name", "group_level",
      "strata_name", "strata_level", "variable_name", "variable_level"
    ),
    names(estimates)
  )

  retained_times <- estimates |>
    dplyr::filter(.data$estimate_name == "estimate") |>
    dplyr::mutate("..time" = suppressWarnings(as.numeric(.data$additional_level))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(curve_columns)), .data$..time) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(curve_columns))) |>
    dplyr::filter(
      dplyr::row_number() == 1L |
        is.na(dplyr::lag(.data$estimate_value)) |
        .data$estimate_value != dplyr::lag(.data$estimate_value)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(curve_columns), "additional_level")

  estimates <- estimates |>
    dplyr::semi_join(retained_times, by = c(curve_columns, "additional_level"))

  filtered <- result |>
    dplyr::filter(!.data$result_id %in% .env$estimate_ids) |>
    dplyr::bind_rows(estimates) |>
    dplyr::arrange(.data$result_id)

  attr(filtered, "settings") <- NULL
  class(filtered) <- setdiff(class(filtered), "summarised_result")
  omopgenerics::newSummarisedResult(filtered, settings = result_settings)
}
