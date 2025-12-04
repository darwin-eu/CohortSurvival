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

#' Helper for consistent documentation of `tables`.
#'
#' @param result A summarised_result object obtained either from
#' `estimateSingleEventSurvival()` or `estimateCompetingRiskSurvival()`.
#' @param header A vector specifying the elements to include in the header.
#' The order of elements matters, with the first being the topmost header.
#' Elements in header can be:
#'  - Any of the columns returned by `tableColumns(result)` to create a header
#' for these columns.
#'  - Any other input to create an overall header.
#' @param hide Columns to drop from the output table. By default, `result_id` and
#' `estimate_type` are always dropped.
#' @param groupColumn Columns to use as group labels, to see options use
#' `tableColumns(result)`. By default, the name of the new group will be the
#' tidy* column names separated by ";". To specify a custom group name, use a
#' named list such as:
#' list("newGroupName" = c("variable_name", "variable_level")).
#'
#' *tidy: The tidy format applied to column names replaces "_" with a space and
#' converts to sentence case. Use `rename` to customise specific column names.
#'
#' @param type Character string specifying the desired output table format.
#' See `tableType()` for supported table types. If `type = NULL`, global
#' options (set via `setGlobalTableOptions()`) will be used if available;
#' otherwise, a default `'gt'` table is created.
#' @param columnOrder Character vector establishing the position of the columns
#' in the formatted table. Columns in either header, groupColumn, or hide will
#' be ignored.
#' @param style Defines the visual formatting of the table.
#' This argument can be provided in one of the following ways:
#' 1. **Pre-defined style:** Use the name of a built-in style (e.g., `"darwin"`).
#' See `tableStyle()` for available options.
#' 2. **YAML file path:** Provide the path to an existing `.yml` file defining
#' a new style.
#' 3. **List of custome R code:** Supply a block of custom R code or a named list
#' describing styles for each table section. This code must be specific to
#' the selected table type.
#' If `style = NULL`, the function will use global options
#' (see `setGlobalTableOptions()`) or an existing `_brand.yml` file (if found);
#' otherwise, the default style is applied.
#' For more details, see the *Styles* vignette on the package website.
#' @param .options A named list with additional formatting options.
#' `visOmopResults::tableOptions()` shows allowed arguments and their default values.
#'
#' @name tableDoc
#' @keywords internal
NULL

#' Table with survival summary
#'
#' @param x Result from estimateSingleEventSurvival or estimateCompetingRiskSurvival
#' @param times Times at which to report survival in the summary table
#' @param timeScale Time unit to report survival in: days, months or years
#' @param estimates Character vector specifying which estimates to include in the table.
#' Options include: "median_survival", "restricted_mean_survival", "q0_survival",
#' "q05_survival", "q25_survival", "q75_survival", "q95_survival", "q100_survival".
#' By default it includes c("median_survival", "restricted_mean_survival").
#' @inheritParams tableDoc
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
                          estimates = c("median_survival", "restricted_mean_survival"),
                          type = "gt",
                          groupColumn = NULL,
                          hide = c("result_id", "estimate_type"),
                          style = NULL,
                          .options = list()){
  rlang::check_installed("visOmopResults", version = "0.5.0")

  # initial checks
  omopgenerics::assertNumeric(times, min = 0, null = TRUE)
  omopgenerics::assertCharacter(timeScale, length = 1)
  omopgenerics::assertChoice(timeScale, c("days", "months", "years"))
  omopgenerics::assertCharacter(estimates)

  # Convert survival result to summarised_result if needed
  if (inherits(x, "survival_result")) {
    x <- asSummarisedResult(x)
  } else if (!inherits(x, "summarised_result")) {
    cli::cli_abort("Input {.arg x} must be a {.cls summarised_result} or {.cls survival_result} object.")
  }

  # Check available result types
  available_types <- omopgenerics::settings(x)$result_type |> unique()

  # Check for required types - use merged types
  has_summary <- "survival_summary" %in% available_types
  has_estimates <- "survival_estimates" %in% available_types

  # Warning if no data available, return empty result table
  if (!has_summary && !has_estimates) {
    cli::cli_warn(c("!" = "No suitable survival data found. Expected {.val survival_summary} and/or survival estimates ({.val survival_estimates}). Returning empty table"))
    return(visOmopResults::emptyTable(type = type, style = style))
  }

  # Warning if times requested but no estimates data available
  if (!is.null(times) && !has_estimates) {
    cli::cli_warn("Time-specific estimates requested but no survival estimates data available. Only summary statistics will be included.")
    times <- NULL
  }

  # Validate estimates parameter
  available_estimates <- c("median_survival", "restricted_mean_survival", "q0_survival",
                           "q05_survival", "q25_survival", "q75_survival", "q95_survival",
                           "q100_survival")
  omopgenerics::assertChoice(estimates, available_estimates)

  # Expand estimates to include confidence intervals
  estimate_names <- estimates
  for (est in estimates) {
    if (est == "median_survival") {
      estimate_names <- c(estimate_names, "median_survival_95CI_lower", "median_survival_95CI_higher")
    } else if (est == "restricted_mean_survival") {
      estimate_names <- c(estimate_names, "restricted_mean_survival_95CI_lower", "restricted_mean_survival_95CI_upper")
    } else if (grepl("^q\\d+_survival$", est)) {
      estimate_names <- c(estimate_names, paste0(est, "_95CI_lower"), paste0(est, "_95CI_higher"))
    }
  }

  userOptions <- .options
  .options <- optionsTableSurvival()
  for (opt in names(userOptions)) {
    .options[[opt]] <- userOptions[[opt]]
  }

  # check times in x
  x_clean <- x |>
    dplyr::filter(.data$result_id %in% (omopgenerics::settings(x) |>
                                          dplyr::filter(grepl("estimates|summary", .data$result_type)) |>
                                          dplyr::pull("result_id"))) |>
    omopgenerics::splitAdditional()

  if("reason_id" %in% colnames(x_clean)) {
    x_clean <- x_clean |>
      dplyr::select(-"reason_id")
  }

  if (!is.null(times)) {
    times_final <- dplyr::tibble(
      name = times,
      value = c(NA)
    )
    if (timeScale == "years") {
      summary_times <- x_clean |>
        dplyr::filter(.data$time != "overall") |>
        dplyr::mutate(time = round(as.numeric(.data$time)/365.25, digits = 3))
    } else if(timeScale == "months"){
      summary_times <- x_clean |>
        dplyr::filter(.data$time != "overall") |>
        dplyr::mutate(time = round(as.numeric(.data$time) / 30.4375, digits = 2))
    } else {
      summary_times <- x_clean
    }

    times_final <- tibble::tibble(name = times, value = times)

    for (t in times) {
      if (timeScale == "days" || t %in% summary_times$time) {
        times_final <- times_final |>
          dplyr::mutate(value = dplyr::if_else(.data$name == t, t, .data$value))
      } else {
        # Define possible rounding adjustments
        if(timeScale == "years") {
          t_plus <- round(t + 0.001, digits = 3)
          t_minus <- round(t - 0.001, digits = 3)
        } else {
          t_plus <- round(t + 0.01, digits = 2)
          t_minus <- round(t - 0.01, digits = 2)
        }

        if (t_plus %in% summary_times$time) {
          times_final <- times_final |>
            dplyr::mutate(value = dplyr::if_else(.data$name == t, t_plus, .data$value))

          cli::cli_alert_info(glue::glue("Because of conversion from days to {timeScale},
                                        the requested estimate for time {t} will be given by {t_plus}."))
        } else if (t_minus %in% summary_times$time) {
          times_final <- times_final |>
            dplyr::mutate(value = dplyr::if_else(.data$name == t, t_minus, .data$value))

          cli::cli_alert_info(glue::glue("Because of conversion from days to {timeScale},
                                        the requested estimate for time {t} will be given by {t_minus}."))
        } else {
          cli::cli_alert_warning(glue::glue("Requested time {t} is not in the survival output.
                                           No estimate will be included in the summary."))
        }
      }
    }

    summary_times <- summary_times |>
      dplyr::left_join(omopgenerics::settings(x_clean) |>
                         dplyr::select(c("result_id", "result_type", "outcome", "competing_outcome")),
                       by = "result_id") |>
      dplyr::filter(.data$time %in% (times_final |>
                                       dplyr::pull("value")),
                    .data$result_type == "survival_estimates")

    if(typeof(summary_times$time) == "character") {
      times_final <- times_final |>
        dplyr::mutate(value = as.character(.data$value))
    }

    if (nrow(summary_times) > 0) {
      summary_times <- summary_times |>
        dplyr::mutate(
          estimate_value = as.character(as.numeric(.data$estimate_value)*100)
        ) |>
        visOmopResults::formatEstimateValue(
          decimals = .options$decimals,
          decimalMark = .options$decimalMark,
          bigMark = .options$bigMark
        ) |>
        visOmopResults::formatEstimateName(
          estimateName =
            c(" survival estimate" =
                "<estimate> (<estimate_95CI_lower>, <estimate_95CI_upper>)")
        ) |>
        dplyr::left_join(times_final, by = c("time" = "value")) |>
        dplyr::mutate(
          "estimate_name" = paste0(.data$name, " ", .env$timeScale, .data$estimate_name)
        )
    }
  }

  if("time"%in% colnames(x_clean)) {
    summary_table <- x_clean |>
      dplyr::filter(
        .data$estimate_name %in% c("number_records_count", "n_events_count", estimate_names),
        .data$time == "overall"
      ) |>
      dplyr::select(!c("time")) |>
      dplyr::mutate(
        "estimate_name" = dplyr::case_when(
          .data$estimate_name == "n_events_count" ~ "Number events",
          .data$estimate_name == "number_records_count" ~ "Number records",
          .default = .data$estimate_name
        ),
        "estimate_type" = dplyr::if_else(
          grepl("Number", .data$estimate_name), "integer", .data$estimate_type
        )

      ) |>
      dplyr::mutate(
        "estimate_name" = factor(
          .data$estimate_name,
          levels = c("Number records", "Number events", "median_survival",
                     "median_survival_95CI_lower", "median_survival_95CI_higher",
                     "restricted_mean_survival", "restricted_mean_survival_95CI_upper",
                     "restricted_mean_survival_95CI_lower", "q0_survival",
                     "q0_survival_95CI_lower", "q0_survival_95CI_higher",
                     "q05_survival", "q05_survival_95CI_lower", "q05_survival_95CI_higher",
                     "q25_survival", "q25_survival_95CI_lower", "q25_survival_95CI_higher",
                     "q75_survival", "q75_survival_95CI_lower", "q75_survival_95CI_higher",
                     "q95_survival", "q95_survival_95CI_lower", "q95_survival_95CI_higher",
                     "q100_survival", "q100_survival_95CI_lower", "q100_survival_95CI_higher"))
      ) |>
      dplyr::arrange(.data$estimate_name) |>
      dplyr::mutate("estimate_name" = as.character(.data$estimate_name))
  } else {
    summary_table <- x_clean |>
      dplyr::filter(
        .data$estimate_name %in% c("number_records_count", "n_events_count", estimate_names)
      ) |>
      dplyr::mutate(
        "estimate_name" = dplyr::case_when(
          .data$estimate_name == "n_events_count" ~ "Number events",
          .data$estimate_name == "number_records_count" ~ "Number records",
          .default = .data$estimate_name
        ),
        "estimate_type" = dplyr::if_else(
          grepl("Number", .data$estimate_name), "integer", .data$estimate_type
        )
      ) |>
      dplyr::mutate(
        "estimate_name" = factor(
          .data$estimate_name,
          levels = c("Number records", "Number events", "median_survival",
                     "median_survival_95CI_lower", "median_survival_95CI_higher",
                     "restricted_mean_survival", "restricted_mean_survival_95CI_upper",
                     "restricted_mean_survival_95CI_lower", "q0_survival",
                     "q0_survival_95CI_lower", "q0_survival_95CI_higher",
                     "q05_survival", "q05_survival_95CI_lower", "q05_survival_95CI_higher",
                     "q25_survival", "q25_survival_95CI_lower", "q25_survival_95CI_higher",
                     "q75_survival", "q75_survival_95CI_lower", "q75_survival_95CI_higher",
                     "q95_survival", "q95_survival_95CI_lower", "q95_survival_95CI_higher",
                     "q100_survival", "q100_survival_95CI_lower", "q100_survival_95CI_higher"))
      ) |>
      dplyr::arrange(.data$estimate_name) |>
      dplyr::mutate("estimate_name" = as.character(.data$estimate_name))
  }

  if(timeScale == "years") {
    summary_table <- summary_table |>
      dplyr::mutate(
        "estimate_value" = dplyr::if_else(grepl("mean", .data$estimate_name) |
                                            grepl("median", .data$estimate_name) |
                                            grepl("survival", .data$estimate_name),
                                          as.character(round(as.numeric(.data$estimate_value)/365.25,3)),
                                          .data$estimate_value)
      )
  }
  if(timeScale == "months") {
    summary_table <- summary_table |>
      dplyr::mutate(
        "estimate_value" = dplyr::if_else(grepl("mean", .data$estimate_name) |
                                            grepl("median", .data$estimate_name) |
                                            grepl("survival", .data$estimate_name),
                                          as.character(round(as.numeric(.data$estimate_value)/30.4375,3)),
                                          .data$estimate_value)
      )
  }

  summary_table <- summary_table |>
    dplyr::left_join(
      omopgenerics::settings(summary_table) |>
        dplyr::select("result_id", "result_type", "outcome", "competing_outcome"),
      by = "result_id"
    )

  if (!is.null(times)) {
    if(summary_times |> dplyr::tally() |> dplyr::pull() != 0) {
      summary_table <- summary_table |>
        dplyr::bind_rows(summary_times |> dplyr::select(!c("name","time")))
    }
  }

  excludeCols <- c("result_id", "estimate_type")

  # Check for competing outcomes using competing_outcome column instead of result_type
  has_competing <- any(summary_table$competing_outcome != "none", na.rm = TRUE)

  if (has_competing) {
    summary_table <- summary_table |>
      dplyr::mutate(
        "variable_name" = dplyr::if_else(
          .data$variable_level == .data$outcome, "outcome", "competing_outcome"
        )
      )
    renameCols <- c(
      "Outcome type" = "variable_name",
      "Outcome name" = "variable_level"
    )
    formatEstimateName <- c("Restricted mean survival" = "<restricted_mean_survival>")
    hide <- c("time", "reason_id", "reason")
  } else {
    hide <- c(hide, "variable_name","time", "reason_id", "reason")
    renameCols <- c("Outcome name" = "variable_level")
    formatEstimateName <- c("Restricted mean survival (95% CI)" =
                              "<restricted_mean_survival> (<restricted_mean_survival_95CI_lower>, <restricted_mean_survival_95CI_upper>)")
  }
  if ("median_survival" %in% unique(summary_table$estimate_name)) {
    formatEstimateName <- c(
      "Median survival (95% CI)" =
        "<median_survival> (<median_survival_95CI_lower>, <median_survival_95CI_higher>)",
      formatEstimateName
    )
  }

  # Create format estimate names for quantiles
  formatEstimateName <- c()

  if ("median_survival" %in% unique(summary_table$estimate_name)) {
    formatEstimateName <- c(formatEstimateName,
                            "Median survival (95% CI)" = "<median_survival> (<median_survival_95CI_lower>, <median_survival_95CI_higher>)")
  }

  formatEstimateName <- c(formatEstimateName,
                          "Restricted mean survival (95% CI)" = "<restricted_mean_survival> (<restricted_mean_survival_95CI_lower>, <restricted_mean_survival_95CI_upper>)")

  # Add quantile formatting
  quantile_estimates <- c("q0_survival", "q05_survival", "q25_survival", "q75_survival", "q95_survival", "q100_survival")
  quantile_labels <- c("0% quantile (95% CI)", "5% quantile (95% CI)", "25% quantile (95% CI)",
                       "75% quantile (95% CI)", "95% quantile (95% CI)", "100% quantile (95% CI)")

  for (i in seq_along(quantile_estimates)) {
    if (quantile_estimates[i] %in% unique(summary_table$estimate_name)) {
      ci_lower <- paste0(quantile_estimates[i], "_95CI_lower")
      ci_higher <- paste0(quantile_estimates[i], "_95CI_higher")
      formatEstimateName <- c(formatEstimateName,
                              stats::setNames(paste0("<", quantile_estimates[i], "> (<", ci_lower, ">, <", ci_higher, ">)"), quantile_labels[i]))
    }
  }

  # to SR
  summary_table <- summary_table |>
    omopgenerics::uniteAdditional() |>
    dplyr::select(omopgenerics::resultColumns())

  summary_table <- visOmopResults::visOmopTable(
    summary_table,
    estimateName = formatEstimateName,
    header = header,
    groupColumn = groupColumn,
    type = type,
    rename = renameCols,
    hide = hide,
    style = style,
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
#' @inheritParams tableDoc
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
                      hide = c("result_id", "estimate_type"),
                      style = NULL,
                      .options = list()){

  # Deprecation warning
  cli::cli_warn(c("!" = "{.fn riskTable} has been renamed to {.fn tableSurvivalEvents}.",
                  "i" = "The current function name will be deprecated in a future version.",
                  "i" = "Please use {.fn tableSurvivalEvents} instead."))

  return(tableSurvivalEvents(x = x,
                             eventGap = eventGap,
                             header = header,
                             type = type,
                             groupColumn = groupColumn,
                             hide = hide,
                             style = style,
                             .options = .options))
}

#' Table with survival events
#'
#' @param x Result from estimateSingleEventSurvival or estimateCompetingRiskSurvival.
#' @param eventGap Event gap defining the times at which to report the risk table
#' information. Must be one of the eventGap inputs used for the estimation function.
#' If NULL, all available are reported.
#' @inheritParams tableDoc
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
#' tableSurvivalEvents(surv)
#'}
tableSurvivalEvents <- function(x,
                                eventGap = NULL,
                                header = c("estimate"),
                                type = "gt",
                                groupColumn = NULL,
                                hide = c("result_id", "estimate_type"),
                                style = NULL,
                                .options = list()){

  rlang::check_installed("visOmopResults", version = "0.5.0")

  # Initial checks
  omopgenerics::assertNumeric(eventGap, min = 0, null = TRUE)

  # Convert survival result to summarised_result if needed
  if (inherits(x, "survival_result")) {
    x <- asSummarisedResult(x)
  } else if (!inherits(x, "summarised_result")) {
    cli::cli_abort("Input {.arg x} must be a {.cls summarised_result} or {.cls survival_result} object.")
  }

  # Check available result types
  available_types <- omopgenerics::settings(x)$result_type |> unique()

  # Check for survival_events type
  has_events <- "survival_events" %in% available_types

  if (!has_events) {
    cli::cli_abort("No survival events data found. Expected {.val survival_events} result type for risk table generation.")
  }

  userOptions <- .options
  .options <- optionsTableSurvival()
  for (opt in names(userOptions)) {
    .options[[opt]] <- userOptions[[opt]]
  }

  # get table of events
  x_clean <- x |>
    dplyr::filter(.data$result_id %in% (omopgenerics::settings(x) |>
                                          dplyr::filter(.data$result_type == "survival_events") |>
                                          dplyr::pull("result_id"))) |>
    omopgenerics::splitAdditional() |>
    dplyr::select(-dplyr::any_of("reason_id")) |>
    dplyr::left_join(omopgenerics::settings(x) |>
                       dplyr::select(c("result_id","eventgap")),
                     by = "result_id")

  if(!is.null(eventGap)) {
    x_clean <- x_clean |>
      dplyr::filter(.data$eventgap %in% eventGap)

    if(x_clean |> dplyr::tally() |> dplyr::pull() == 0) {
      cli::cli_abort("No events for the specified eventGap {eventGap}")
    }
  }

  events_table <- x_clean |>
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
    ) |>
    dplyr::mutate(
      "estimate_name" = factor(
        .data$estimate_name,
        levels = c("Number at risk", "Number events", "Number censored"))
    ) |>
    dplyr::arrange(.data$estimate_name) |>
    dplyr::mutate("estimate_name" = as.character(.data$estimate_name))

  events_table <- events_table |>
    dplyr::left_join(
      omopgenerics::settings(x) |>
        dplyr::select("result_id", "result_type", "outcome", "competing_outcome"),
      by = "result_id"
    )


  excludeCols <- c("result_id", "estimate_type")

  # Check for competing outcomes using competing_outcome column
  has_competing <- any(events_table$competing_outcome != "none", na.rm = TRUE)

  if (has_competing) {
    events_table <- events_table |>
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
    hide <- c("reason_id", "reason")
  } else {
    hide <- c(hide, "variable_name", "reason_id", "reason")
    renameCols <- c("Outcome name" = "variable_level",
                    "Time" = "time",
                    "Event gap" = "eventgap")
  }

  # to SR
  events_table <- events_table |>
    dplyr::mutate(additional_level = paste0(.data$time, " &&& ", .data$eventgap)) |>
    dplyr::mutate(additional_name = "time &&& eventgap") |>
    dplyr::select(c(omopgenerics::resultColumns()))

  events_table <- visOmopResults::visOmopTable(
    events_table,
    header = header,
    groupColumn = groupColumn,
    settingsColumn = "eventgap",
    type = type,
    rename = renameCols,
    hide = hide,
    style = style,
    .options = c(.options, list(useFormatOrder = FALSE)) # to keep order set when factoring
  )

  return(events_table)
}

#' Display the attrition of a survival result in a visual table
#'
#' @inheritParams tableDoc
#'
#' @return A visual table
#' @export
#'
#' @examples
#' \donttest{
#' library(CohortSurvival)
#'
#' cdm <- mockMGUS2cdm()
#'
#' surv <- estimateSingleEventSurvival(
#'   cdm = cdm,
#'   targetCohortTable = "mgus_diagnosis",
#'   outcomeCohortTable = "death_cohort"
#' )
#'
#' tableSurvivalAttrition(surv)
#' }
#'
tableSurvivalAttrition <- function(result,
                                   type = "gt",
                                   header = "variable_name",
                                   groupColumn = c("cdm_name", "target_cohort", "variable_level"),
                                   hide = c("estimate_name"),
                                   style = NULL,
                                   .options = list()) {
  rlang::check_installed("visOmopResults", version = "1.4.0")

  # Convert survival result to summarised_result if needed
  if (inherits(result, "survival_result")) {
    result <- asSummarisedResult(result)
  } else if (!inherits(result, "summarised_result")) {
    cli::cli_abort("Input {.arg x} must be a {.cls summarised_result} or {.cls survival_result} object.")
  }
  result <- omopgenerics::validateResultArgument(result = result) |>
    omopgenerics::filterSettings(.data$result_type == "survival_attrition")

  if (nrow(result) == 0) {
    cli::cli_warn(c("!" = "No `survival_attrition` results found, returning empty table."))
    return(visOmopResults::emptyTable(type = type, style = style))
  }

  userOptions <- .options
  .options <- optionsTableSurvival()
  for (opt in names(userOptions)) {
    .options[[opt]] <- userOptions[[opt]]
  }

  visOmopResults::visOmopTable(
    result = result,
    header = header,
    groupColumn = groupColumn,
    hide = hide,
    type = type,
    style = style,
    columnOrder = c(
      "cdm_name", "target_cohort", "reason_id",
      omopgenerics::strataColumns(result = result), "variable_level",
      "variable_name", "estimate_name"
    )
  )
}

