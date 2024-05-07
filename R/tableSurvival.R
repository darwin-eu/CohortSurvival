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
#' @param timeScale Time unit to report survival in: days, months or years
#' @param splitStrata If TRUE strata will be split into columns, otherwise
#' "strata_name" and "strata_level" columns will be kept.
#' @param header A vector containing which elements should go into the header.
#' Allowed are: cdm_name, group, strata, additional, variable, estimate,
#' and settings.
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable",  and "tibble".
#' @param groupColumn Columns to use as group labels.
#' @param .options Named list with additional formatting options.
#' CohortSurvival::optionsTableSurvival() shows allowed arguments and their
#' default values.
#'
#' @return A tibble containing a summary of observed survival in the required units
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
                          times = NULL,
                          timeScale = "days",
                          splitStrata = TRUE,
                          header = c("estimate"),
                          type = "gt",
                          groupColumn = NULL,
                          .options = list()){

  # initial checks
  checkmate::assert_numeric(times,
                            lower = 0,
                            null.ok = TRUE)
  checkmate::assert_character(timeScale,
                              len = 1)
  if(!(timeScale %in% c("days", "years"))){
    cli::cli_abort(paste0("The input `timeScale` must be `days` or `years`
                          but it is `",timeScale,"`"))
  }

  # .options:
  userOptions <- .options
  .options = optionsTableSurvival()
  for (opt in names(userOptions)) {
    .options[[opt]] <- userOptions[[opt]]
  }

  # check times in x
  x_clean <- x %>% visOmopResults::splitAdditional()

  if (!is.null(times)) {
    if (timeScale == "years") {
      summary_times <- x_clean %>%
        dplyr::filter(.data$time != "overall") %>%
        dplyr::mutate(time = round(as.numeric(.data$time)/364.25, digits = 3))
    } else {
      summary_times <- x_clean
    }
    for (t in times) {
      if (!(t %in% summary_times$time)) {
        if ((round(t + 0.001, digits = 3) %in% summary_times$time & timeScale == "years")) {
          times[times == t] <- round(t + 0.001, digits = 3)
          cli::cli_alert(paste0("Because of the conversion from days to years,
          the requested time ",t," has now been changed to ",t + 0.001))
        } else if ((round(t - 0.001, digits = 3) %in% summary_times$time & timeScale == "years")) {
          times[times == t] <- round(t - 0.001, digits = 3)
          cli::cli_alert(paste0("Because of the conversion from days to years,
          the requested time ",t," has now been changed to ",t - 0.001))
        } else {
          cli::cli_alert(paste0("Requested time ",t," is not in the list of times
                         of the survival output provided, so no estimate
                         will be included in the summary"))
        }
      }
    }

    summary_times <- summary_times %>%
      dplyr::filter(.data$time %in% .env$times)

    if (nrow(summary_times) > 0) {
      summary_times <- summary_times %>%
        dplyr::mutate(
          estimate_value = as.character(as.numeric(.data$estimate_value)*100)
        ) %>%
        visOmopResults::formatEstimateValue(
          decimals = .options$decimals,
          decimalMark = .options$decimalMark,
          bigMark = .options$bigMark
        ) %>%
        visOmopResults::formatEstimateName(
          estimateNameFormat =
            c(" survival estimate" =
                "<estimate> (<estimate_95CI_lower>, <estimate_95CI_upper>)")
        ) %>%
        dplyr::mutate(
          "estimate_name" = paste0(.data$time, " ", .env$timeScale, .data$estimate_name)
        )
    }
  }

  summary_table <- x_clean %>%
    dplyr::filter(
      .data$estimate_name %in%
        c("median_survival", "number_records", "n_events",
          "median_survival_95CI_lower", "median_survival_95CI_higher",
          "restricted_mean_survival", "restricted_mean_survival_se"),
      .data$time == "overall"
    ) %>%
    dplyr::select(!"time") %>%
    dplyr::mutate(
      "estimate_name" = dplyr::case_when(
        .data$estimate_name == "n_events" ~ "Number events",
        .data$estimate_name == "number_records" ~ "Number records",
        .default = .data$estimate_name
      ),
      "estimate_type" = dplyr::if_else(
        grepl("Number", .data$estimate_name), "integer", .data$estimate_type
      )
    ) %>%
    dplyr::mutate(
      "estimate_name" = factor(
        .data$estimate_name,
        levels = c("Number records", "Number events", "median_survival",
                   "median_survival_95CI_lower", "median_survival_95CI_higher",
                   "restricted_mean_survival", "restricted_mean_survival_se"))
    ) %>%
    dplyr::arrange(.data$estimate_name) %>%
    dplyr::mutate("estimate_name" = as.character(.data$estimate_name))

  if (!is.null(times)) {
    summary_table <- summary_table %>%
      dplyr::bind_rows(summary_times %>% dplyr::select(!"time"))
  }


  split <- c("group", "additional")
  if (splitStrata) {
    split = c(split, "strata")
  }

  excludeCols <- c("result_id", "estimate_type")

  if ("competing_outcome" %in% visOmopResults::additionalColumns(x)) {
    summary_table <- summary_table %>%
      dplyr::mutate(
        "variable_name" = dplyr::if_else(
          .data$variable_level == .data$outcome, "outcome", "competing_outcome"
        )
      )
    renameCols = c(
      "Outcome type" = "variable_name",
      "Outcome name" = "variable_level"
    )
    formatEstimateName = c("Restricted mean survival" = "<restricted_mean_survival>")
  } else {
    excludeCols = c(excludeCols, "variable_name")
    renameCols = c("Outcome name" = "variable_level")
    formatEstimateName = c("Restricted mean survival (SE)" =
                             "<restricted_mean_survival> (<restricted_mean_survival_se>)")
  }
  if ("median_survival" %in% unique(summary_table$estimate_name)) {
    formatEstimateName = c(
      "Median survival (95% CI)" =
        "<median_survival> (<median_survival_95CI_lower>, <median_survival_95CI_higher>)",
      formatEstimateName
    )
  }

  # to SR
  summary_table <- summary_table %>%
    visOmopResults::uniteAdditional() %>%
    dplyr::select(omopgenerics::resultColumns())

  summary_table <- visOmopResults::visOmopTable(
    summary_table,
    formatEstimateName = formatEstimateName,
    header = header,
    split = split,
    groupColumn = groupColumn,
    type = type,
    renameColumns = renameCols,
    showMinCellCount = TRUE,
    excludeColumns = excludeCols,
    .options = c(.options, list(useFormatOrder = FALSE)) # to keep order set when factoring
  )

  return(summary_table)
}

#' Additional arguments for the function tableSurvival()
#'
#' @description
#' It provides a list of allowed inputs for .option argument in
#' tableSurvival and their given default value.
#'
#'
#' @return The default .options named list.
#'
#' @export
#'
#' @examples
#' {
#' optionsTableSurvival()
#' }
#'
#'
optionsTableSurvival <- function() {
  default <- visOmopResults::optionsVisOmopTable()
  default <- default[!names(default) %in% c("useFormatOrder", "keepNotFormatted")]
  return(default)
}
