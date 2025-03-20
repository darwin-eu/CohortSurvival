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
                          header = c("estimate"),
                          type = "gt",
                          groupColumn = NULL,
                          .options = list()){

  rlang::check_installed("visOmopResults", version = "0.5.0")

  # initial checks
  omopgenerics::assertNumeric(times, min = 0, null = TRUE)
  omopgenerics::assertCharacter(timeScale, length = 1)
  omopgenerics::assertChoice(timeScale, c("days", "years"))

  # .options:
  userOptions <- .options
  .options <- optionsTableSurvival()
  for (opt in names(userOptions)) {
    .options[[opt]] <- userOptions[[opt]]
  }

  # check times in x
  x_clean <- x %>%
    dplyr::filter(.data$result_id %in% (omopgenerics::settings(x) %>%
                    dplyr::filter(grepl("probability", .data$result_type) | grepl("summary", .data$result_type)) %>%
                    dplyr::pull("result_id"))) %>%
    omopgenerics::splitAdditional() %>%
    dplyr::select(-"reason_id")

  if (!is.null(times)) {
    times_final <- dplyr::tibble(
      name = times,
      value = c(NA)
    )
    if (timeScale == "years") {
      summary_times <- x_clean %>%
        dplyr::filter(.data$time != "overall") %>%
        dplyr::mutate(time = round(as.numeric(.data$time)/365.25, digits = 3))
    } else {
      summary_times <- x_clean
    }
    for (t in times) {
      if (!(t %in% summary_times$time)) {
        if ((round(t + 0.001, digits = 3) %in% summary_times$time & timeScale == "years")) {
          times_final <- times_final %>%
            dplyr::mutate(value = dplyr::if_else(.data$name == t,
                                                 round(t + 0.001, digits = 3),
                                                 .data$value))
          cli::cli_alert(paste0("Because of the conversion from days to years,
          the requested estimate for time ",t," will be given by ",t + 0.001))
        } else if ((round(t - 0.001, digits = 3) %in% summary_times$time & timeScale == "years")) {
          times_final <- times_final %>%
            dplyr::mutate(value = dplyr::if_else(.data$name == t,
                                                 round(t - 0.001, digits = 3),
                                                 .data$value))
          cli::cli_alert(paste0("Because of the conversion from days to years,
          the requested estimate for time ",t," will be given by ",t - 0.001))
        } else {
          cli::cli_alert(paste0("Requested time ",t," is not in the list of times
                         of the survival output provided, so no estimate
                         will be included in the summary"))
        }
      }
      if(t %in% summary_times$time) {
        times_final <- times_final %>%
          dplyr::mutate(value = dplyr::if_else(.data$name == t,
                                               t,
                                               .data$value))
      }
    }

    summary_times <- summary_times %>%
      dplyr::left_join(omopgenerics::settings(x_clean) %>%
                         dplyr::select(c("result_id", "result_type", "outcome", "competing_outcome")),
                       by = "result_id") %>%
      dplyr::filter(.data$time %in% (times_final %>%
                      dplyr::pull("value")),
                    grepl("probability", .data$result_type))

    if(typeof(summary_times$time) == "character") {
      times_final <- times_final %>%
        dplyr::mutate(value = as.character(.data$value))
    }

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
          estimateName =
            c(" survival estimate" =
                "<estimate> (<estimate_95CI_lower>, <estimate_95CI_upper>)")
        ) %>%
        dplyr::left_join(times_final, by = c("time" = "value")) %>%
        dplyr::mutate(
          "estimate_name" = paste0(.data$name, " ", .env$timeScale, .data$estimate_name)
        )
    }
  }

  summary_table <- x_clean %>%
    dplyr::filter(
      .data$estimate_name %in%
        c("median_survival", "number_records_count", "n_events_count",
          "median_survival_95CI_lower", "median_survival_95CI_higher",
          "restricted_mean_survival", "restricted_mean_survival_se"),
      .data$time == "overall"
    ) %>%
    dplyr::select(!c("time")) %>%
    dplyr::mutate(
      "estimate_name" = dplyr::case_when(
        .data$estimate_name == "n_events_count" ~ "Number events",
        .data$estimate_name == "number_records_count" ~ "Number records",
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

  if(timeScale == "years") {
    summary_table <- summary_table %>%
      dplyr::mutate(
        "estimate_value" = dplyr::if_else(grepl("mean", .data$estimate_name) | grepl("median", .data$estimate_name),
                                        as.character(round(as.numeric(.data$estimate_value)/365.25,3)),
                                        .data$estimate_value)
      )
  }

  summary_table <- summary_table %>%
    dplyr::left_join(
      omopgenerics::settings(summary_table) %>%
        dplyr::select("result_id", "result_type", "outcome", "competing_outcome"),
      by = "result_id"
    )

  if (!is.null(times)) {
    summary_table <- summary_table %>%
      dplyr::bind_rows(summary_times %>% dplyr::select(!c("name","time")))
  }

  excludeCols <- c("result_id", "estimate_type")

  if ((summary_table %>% dplyr::pull("competing_outcome") %>% unique()) != "none") {
    summary_table <- summary_table %>%
      dplyr::mutate(
        "variable_name" = dplyr::if_else(
          .data$variable_level == .data$outcome, "outcome", "competing_outcome"
        )
      )
    renameCols <- c(
      "Outcome type" = "variable_name",
      "Outcome name" = "variable_level"
    )
    excludeCols <- c("time", "reason_id", "reason")
    formatEstimateName <- c("Restricted mean survival" = "<restricted_mean_survival>")
  } else {
    excludeCols <- c(excludeCols, "variable_name","time", "reason_id", "reason")
    renameCols <- c("Outcome name" = "variable_level")
    formatEstimateName <- c("Restricted mean survival (SE)" =
                             "<restricted_mean_survival> (<restricted_mean_survival_se>)")
  }
  if ("median_survival" %in% unique(summary_table$estimate_name)) {
    formatEstimateName <- c(
      "Median survival (95% CI)" =
        "<median_survival> (<median_survival_95CI_lower>, <median_survival_95CI_higher>)",
      formatEstimateName
    )
  }

  # to SR
  summary_table <- summary_table %>%
    omopgenerics::uniteAdditional() %>%
    dplyr::select(omopgenerics::resultColumns())

  summary_table <- visOmopResults::visOmopTable(
    summary_table,
    estimateName = formatEstimateName,
    header = header,
    groupColumn = groupColumn,
    type = type,
    rename = renameCols,
    hide = excludeCols,
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
  default <- visOmopResults::tableOptions()
  default <- default[!names(default) %in% c("useFormatOrder", "keepNotFormatted")]
  return(default)
}

#' Table with survival events
#'
#' @param x Result from estimateSingleEventSurvival or estimateCompetingRiskSurvival.
#' @param eventGap Event gap defining the times at which to report the risk table
#' information. Must be one of the eventGap inputs used for the estimation function.
#' If NULL, all available are reported.
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
#' @return A tibble containing the risk table information (n_risk, n_events, n_censor)
#' for all times within the event gap specified.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(cdm,
#'                                     targetCohortTable = "mgus_diagnosis",
#'                                     outcomeCohortTable = "death_cohort")
#' riskTable(surv)
#'}
riskTable <- function(x,
                      eventGap = NULL,
                      header = c("estimate"),
                      type = "gt",
                      groupColumn = NULL,
                      .options = list()){

  rlang::check_installed("visOmopResults", version = "0.5.0")

  # Initial checks
  x <- omopgenerics::newSummarisedResult(x)
  omopgenerics::assertNumeric(eventGap, min = 0, null = TRUE)

  # .options:
  userOptions <- .options
  .options <- optionsTableSurvival()
  for (opt in names(userOptions)) {
    .options[[opt]] <- userOptions[[opt]]
  }

  # get table of events
  x_clean <- x %>%
    dplyr::filter(.data$result_id %in% (omopgenerics::settings(x) %>%
                                          dplyr::filter(grepl("events", .data$result_type)) %>%
                                          dplyr::pull("result_id"))) %>%
    omopgenerics::splitAdditional() %>%
    dplyr::select(-"reason_id") %>%
    dplyr::left_join(settings(x) %>%
                       dplyr::select(c("result_id","eventgap")),
                     by = "result_id")

  if(!is.null(eventGap)) {
    x_clean <- x_clean %>%
      dplyr::filter(.data$eventgap %in% eventGap)

    if(x_clean %>% dplyr::tally() %>% dplyr::pull() == 0) {
      cli::cli_abort("No events for the specified eventGap {eventGap}")
    }
  }

  events_table <- x_clean %>%
    dplyr::mutate(
      "estimate_name" = dplyr::case_when(
        .data$estimate_name == "n_risk_count" ~ "Number at risk",
        .data$estimate_name == "n_events_count" ~ "Number events",
        .data$estimate_name == "n_censor_count" ~ "Number censored",
        .default = .data$estimate_name
      ),
      "estimate_type" = dplyr::if_else(
        grepl("Number", .data$estimate_name), "integer", .data$estimate_type
      )
    ) %>%
    dplyr::mutate(
      "estimate_name" = factor(
        .data$estimate_name,
        levels = c("Number at risk", "Number events", "Number censored"))
    ) %>%
    dplyr::arrange(.data$estimate_name) %>%
    dplyr::mutate("estimate_name" = as.character(.data$estimate_name))

  events_table <- events_table %>%
    dplyr::left_join(
      omopgenerics::settings(x) %>%
        dplyr::select("result_id", "result_type", "outcome", "competing_outcome"),
      by = "result_id"
    )

  excludeCols <- c("result_id", "estimate_type")

  if ((events_table %>% dplyr::pull("competing_outcome") %>% unique()) != "none") {
    events_table <- events_table %>%
      dplyr::mutate(
        "variable_name" = dplyr::if_else(
          .data$variable_level == .data$outcome, "outcome", "competing_outcome"
        )
      )
    renameCols <- c(
      "Outcome type" = "variable_name",
      "Outcome name" = "variable_level",
      "Time" = "time",
      "Event gap" = "eventgap"
    )
    excludeCols <- c("reason_id", "reason")
  } else {
    excludeCols <- c(excludeCols, "variable_name", "reason_id", "reason")
    renameCols <- c("Outcome name" = "variable_level",
                    "Time" = "time",
                    "Event gap" = "eventgap")
  }

  # to SR
  events_table <- events_table %>%
    dplyr::mutate(additional_level = paste0(.data$time, " &&& ", .data$eventgap)) %>%
    dplyr::mutate(additional_name = "time &&& eventgap") %>%
    dplyr::select(c(omopgenerics::resultColumns()))

  events_table <- visOmopResults::visOmopTable(
    events_table,
    header = header,
    groupColumn = groupColumn,
    settingsColumn = "eventgap",
    type = type,
    rename = renameCols,
    hide = excludeCols,
    .options = c(.options, list(useFormatOrder = FALSE)) # to keep order set when factoring
  )

  return(events_table)
}
