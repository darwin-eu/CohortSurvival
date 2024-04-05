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


#' Table with survival summary
#'
#' @param x Result from estimateSingleEventSurvival or estimateCompetingRiskSurvival
#' @param times Times at which to report survival in the summary table
#'
#' @return A tibble containing a summary of observed survival in days
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(cdm,
#'                                     targetCohortTable = "mgus_diagnosis",
#'                                     outcomeCohortTable = "death_cohort")
#' tableSurvival(surv, times = c(50,100,365))
#'}
tableSurvival <- function(x,
                          times = NULL){

  # check times
  x_clean <- x %>%
    asSurvivalResult()
  if(!is.null(times)){
  if(!(all(times %in% (x_clean %>%
       dplyr::pull("time"))))) {
    cli::cli_abort("Some of the summary estimates are not available in the survival
    output provided for the times requested for summary reporting. Please change the times
                   argument to available timepoints.")
  }}

  summary_table <- attr(x_clean, "summary")
  summary_times <- x_clean %>%
    dplyr::filter(.data$time %in% .env$times)

  summary_table <- summary_table %>%
    tidyr::pivot_wider(
      names_from = "estimate_name",
      values_from = "estimate_value")

 strata_cols <- setdiff(colnames(summary_table %>%
                    visOmopResults::splitStrata()),
          colnames(summary_table))

 summary_table <- summary_table %>%
   visOmopResults::splitStrata()

  summary_times <- summary_times %>%
    tidyr::pivot_wider(
      names_from = "estimate_name",
      values_from = "estimate_value")

  if(all(c("median_survival") %in% colnames(summary_table))) {
    summary_table <- summary_table %>%
      dplyr::mutate("Median survival (95% CI)" = paste0(.data$median_survival, " (",
                                                        .data$median_survival_95CI_lower, ", ",
                                                        .data$median_survival_95CI_higher, ")"))
  }

    summary_table <- summary_table %>%
      dplyr::select(dplyr::any_of(c("result_id", "cdm_name",
                                   "cohort", "variable_level",
        "analysis_type", "outcome", "competing_outcome",
        strata_cols,
        "number_records",
        "events", "Median survival (95% CI)"
      )))

  all_cols <- c("cohort", "outcome", "competing_outcome",
                "strata_name", "strata_level", "variable_level")[
                  which(c("cohort", "outcome", "competing_outcome",
                "strata_name", "strata_level", "variable_level") %in%
    colnames(summary_times))]

  if(nrow(summary_times) > 0){
  summary_times <- summary_times %>%
    dplyr::left_join(
      summary_times %>%
        dplyr::mutate(estimate = paste0(.data$estimate, " (",
                                        .data$estimate_95CI_lower, ", ",
                                        .data$estimate_95CI_upper, ")")) %>%
        dplyr::mutate(
          time = paste0(.data$time, " days survival estimate")) %>%
        tidyr::pivot_wider(id_cols = dplyr::all_of(all_cols),
                           names_from = "time",
                           values_from = "estimate"),
      by = all_cols
    ) %>%
    dplyr::select(-tidyr::contains("95"), -"time", -"estimate",
                  -"result_type", -"variable_name") %>%
    visOmopResults::splitStrata()

  cols <- summary_times %>%
    dplyr::select(-tidyr::contains("survival")) %>%
    colnames()

  summary_table <- summary_table %>%
    dplyr::left_join(
      summary_times,
      by = cols
    )
  }

  summary_table <- summary_table %>%
   dplyr::distinct() %>%
   dplyr::select(!"result_id")

  return(summary_table)
}
