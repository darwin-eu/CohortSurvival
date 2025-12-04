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
#' ) |>
#' asSurvivalResult()
#' }
#'
#' @return A tibble with a tidy version of the summarised_characteristics
#' object.
#' @export
#'
asSurvivalResult <- function(result) {
  validation <- validateSurvivalResult(result)
  available_types <- validation$available_types

  # Base required columns (always expected)
  base_required_cols <- c("result_id","cdm_name", "group_name", "group_level", "strata_name",
                         "strata_level", "variable_name", "variable_level",
                         "estimate_name", "estimate_type", "estimate_value",
                         "additional_name", "additional_level", "result_type",
                         "outcome", "competing_outcome")

  # Optional columns that may or may not be present
  optional_cols <- c("eventgap")

  # Only include optional columns if they exist in the data
  available_optional_cols <- intersect(optional_cols, names(result))
  required_cols <- c(base_required_cols, available_optional_cols)

  result <- result |>
     omopgenerics::addSettings(
       settingsColumn = c("result_type",omopgenerics::settingsColumns(result)))

  non_constant_cols <- result |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.))) |>
    purrr::keep(~ .x > 1) |>
    names()

  if (length(non_constant_cols[!(non_constant_cols %in% required_cols)]) > 0) {
    cli::cli_warn("{.strong {non_constant_cols[!(non_constant_cols %in% required_cols)]}} column{?s} will be added to the survival result object to include all relevant information")
  }

  final_cols <- dplyr::union(required_cols, non_constant_cols)

  # Store the original settings before processing
  original_settings <- omopgenerics::settings(result)

  result <- result |>
    dplyr::select(dplyr::any_of(final_cols)) |>
    omopgenerics::splitAdditional() |>
    omopgenerics::splitGroup() |>
    omopgenerics::splitStrata()

  estimates <- NULL
  summary <- NULL
  events <- NULL
  attrition <- NULL

  if ("survival_estimates" %in% available_types) {
    estimates <- result |>
      dplyr::filter(.data$result_type == "survival_estimates") |>
      dplyr::select(-dplyr::any_of(c("eventgap", "reason_id", "reason", "result_id"))) |>
      dplyr::mutate(time = as.numeric(.data$time)) |>
        dplyr::relocate("outcome", .after = "target_cohort") |>
        dplyr::relocate("competing_outcome", .after = "outcome") |>
      omopgenerics::pivotEstimates() |>
      dplyr::select(-dplyr::starts_with("variable_name")) |>
      dplyr::rename("variable" = "variable_level")
  }

  if ("survival_summary" %in% available_types) {
    summary <- result |>
      dplyr::filter(.data$result_type == "survival_summary") |>
      dplyr::select(-dplyr::any_of(c("variable_name", "time", "eventgap", "reason_id", "result_id", "reason"))) |>
      dplyr::mutate(estimate_name = dplyr::if_else(
        grepl("count", .data$estimate_name),
        gsub("_count","",.data$estimate_name),
        .data$estimate_name
      )) |>
      dplyr::select(-dplyr::starts_with("variable_name")) |>
      dplyr::rename("variable" = "variable_level") |>
      dplyr::relocate("outcome", .after = "target_cohort") |>
      dplyr::relocate("competing_outcome", .after = "outcome") |>
      omopgenerics::pivotEstimates()
  }

  if ("survival_events" %in% available_types) {
    events <- result |>
      dplyr::filter(.data$result_type == "survival_events") |>
      dplyr::select(-dplyr::any_of(c("reason_id", "result_id", "reason"))) |>
      dplyr::distinct() |>
      dplyr::mutate(time = as.numeric(.data$time)) |>
      dplyr::mutate(estimate_name = dplyr::if_else(
        grepl("count", .data$estimate_name),
        gsub("_count","",.data$estimate_name),
        .data$estimate_name
      )) |>
      dplyr::select(-dplyr::starts_with("variable_name")) |>
      dplyr::rename("variable" = "variable_level") |>
      dplyr::relocate("outcome", .after = "target_cohort") |>
      dplyr::relocate("competing_outcome", .after = "outcome") |>
      omopgenerics::pivotEstimates()
  }

  if ("survival_attrition" %in% available_types) {
    attrition <- result |>
      dplyr::filter(.data$result_type == "survival_attrition") |>
      dplyr::select(-dplyr::any_of(c("time", "eventgap", "result_id", "reason_id"))) |>
      dplyr::select(-dplyr::starts_with("variable_level")) |>
      dplyr::relocate("outcome", .after = "target_cohort") |>
      dplyr::relocate("competing_outcome", .after = "outcome") |>
      dplyr::distinct() |>
      omopgenerics::pivotEstimates()
  }

  # Create the survival_result class
  result_final <- list()

  # Return the most relevant data as main object
  if (!is.null(estimates)) {
    result_final <- estimates
  } else if (!is.null(summary)) {
    result_final <- summary
  } else if (!is.null(events)) {
    result_final <- events
  } else if (!is.null(attrition)) {
    result_final <- attrition
  } else {
    cli::cli_warn("No recognized survival data found. Returning original result.")
    return(result)
  }

  # Add attributes for all components
  attr(result_final, "events") <- events
  attr(result_final, "summary") <- summary
  attr(result_final, "attrition") <- attrition

  # Store the original settings attribute to preserve result_id mappings
  attr(result_final, "settings") <- original_settings

  # Add survival_result class
  class(result_final) <- c("survival_result", class(result_final))

  return(result_final)
}

validateSurvivalResult <- function(result, required_types = NULL, allow_any = FALSE) {
  # Check if result is valid summarised_result
  if (!inherits(result, "summarised_result")) {
    cli::cli_abort("result is not a valid `summarised_result` object.")
  }

  # Get available result types
  available_types <- omopgenerics::settings(result)$result_type |> unique()

  # Check if required types are present
  if (!is.null(required_types)) {
    if (allow_any) {
      # At least one of the required types must be present
      if (!any(required_types %in% available_types)) {
        cli::cli_abort("At least one of the required result types {required_types} must be present in the result.")
      }
    } else {
      # All required types must be present
      missing_types <- setdiff(required_types, available_types)
      if (length(missing_types) > 0) {
        cli::cli_abort("Required result types {missing_types} are not present in the result.")
      }
    }
  }

  return(list(
    available_types = available_types,
    is_valid = TRUE
  ))
}

#' Convert survival result back to summarised result
#'
#' Internal function to convert a survival_result object back to a summarised_result format.
#' This is the inverse operation of asSurvivalResult().
#'
#' @param result A survival_result object.
#'
#' @return A summarised_result object.
#' @keywords internal
#'
asSummarisedResult <- function(result) {
  if (!inherits(result, "survival_result")) {
    cli::cli_abort("Input must be a survival_result object.")
  }

  # Get the original settings if available
  original_settings <- attr(result, "settings")
  if (is.null(original_settings)) {
    cli::cli_abort("No settings attribute found in survival_result object. This object may not have been created with asSurvivalResult().")
  }

  # Initialize list to collect all result components
  all_results <- list()

  # Process main result (usually survival_estimates)
  if (!is.null(result) && nrow(result) > 0) {
    main_result <- result |>
      # Convert from pivoted estimates back to long format
      tidyr::pivot_longer(
        cols = c(dplyr::any_of(c("estimate", "estimate_95CI_lower", "estimate_95CI_upper"))),
        names_to = "estimate_name",
        values_to = "estimate_value",
        values_transform = as.character
      ) |>
      dplyr::mutate(
        estimate_type = "numeric",
        result_type = "survival_estimates"
      )

    # Identify stratification columns (exclude known columns)
    known_cols <- c("cdm_name", "target_cohort", "outcome", "competing_outcome", "variable", "time", "result_type", "estimate_name", "estimate_value", "estimate_type")
    strata_cols <- setdiff(names(main_result), known_cols)

    if (length(strata_cols) == 0) {
      # No stratification
      main_result <- main_result |>
        dplyr::mutate(
          strata_name = "overall",
          strata_level = "overall"
        )
    } else {
      # Has stratification columns - create separate rows for each strata combination
      strata_results <- list()

      # Add overall row (where all strata columns are "overall")
      overall_data <- main_result |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(strata_cols), ~ .x == "overall"))

      if (nrow(overall_data) > 0) {
        strata_results[[1]] <- overall_data |>
          dplyr::mutate(
            strata_name = "overall",
            strata_level = "overall"
          ) |>
          dplyr::select(-dplyr::all_of(strata_cols))
      }

      # Add rows for individual strata variables (where one is not "overall", others are "overall")
      for (col in strata_cols) {
        individual_data <- main_result |>
          dplyr::filter(.data[[col]] != "overall" & dplyr::if_all(dplyr::all_of(setdiff(strata_cols, col)), ~ .x == "overall"))

        if (nrow(individual_data) > 0) {
          strata_results[[length(strata_results) + 1]] <- individual_data |>
            dplyr::mutate(
              strata_name = col,
              strata_level = as.character(.data[[col]])
            ) |>
            dplyr::select(-dplyr::all_of(strata_cols))
        }
      }

      # Add rows for multiple strata combinations (where multiple columns are not "overall")
      multi_strata_data <- main_result |>
        dplyr::filter(dplyr::if_any(dplyr::all_of(strata_cols), ~ .x != "overall"))

      if (nrow(multi_strata_data) > 0) {
        multi_strata_result <- multi_strata_data |>
          dplyr::rowwise() |>
          dplyr::mutate(
            strata_name = paste(strata_cols, collapse = " &&& "),
            strata_level = paste(dplyr::c_across(dplyr::all_of(strata_cols)), collapse = " &&& ")
          ) |>
          dplyr::ungroup() |>
          dplyr::select(-dplyr::all_of(strata_cols))

        strata_results[[length(strata_results) + 1]] <- multi_strata_result
      }

      main_result <- dplyr::bind_rows(strata_results)
    }

    main_result <- main_result |>
      # Keep the original column names and structure
      dplyr::mutate(
        group_name = "target_cohort",
        group_level = .data$target_cohort,
        variable_name = "outcome",
        variable_level = .data$variable,
        additional_name = "time",
        additional_level = as.character(.data$time)
      )

    all_results[["estimates"]] <- main_result
  }

  # Process summary attribute
  if (!is.null(attr(result, "summary"))) {
    summary_result <- attr(result, "summary") |>
      tidyr::pivot_longer(
        cols = c(dplyr::any_of(c("median_survival", "median_survival_95CI_lower", "median_survival_95CI_higher",
                                "number_records", "n_events", "restricted_mean_survival",
                                "restricted_mean_survival_95CI_lower", "restricted_mean_survival_95CI_upper",
                                "q0_survival", "q0_survival_95CI_lower", "q0_survival_95CI_higher",
                                "q05_survival", "q05_survival_95CI_lower", "q05_survival_95CI_higher",
                                "q25_survival", "q25_survival_95CI_lower", "q25_survival_95CI_higher",
                                "q75_survival", "q75_survival_95CI_lower", "q75_survival_95CI_higher",
                                "q95_survival", "q95_survival_95CI_lower", "q95_survival_95CI_higher",
                                "q100_survival", "q100_survival_95CI_lower", "q100_survival_95CI_higher",
                                "se(rmean)"))),
        names_to = "estimate_name",
        values_to = "estimate_value",
        values_transform = as.character
      ) |>
      dplyr::mutate(
        estimate_type = "numeric",
        estimate_name = dplyr::if_else(
          grepl("number_records|n_events", .data$estimate_name),
          paste0(.data$estimate_name, "_count"),
          .data$estimate_name
        ),
        result_type = "survival_summary"
      )

    # Identify stratification columns (exclude known columns)
    known_cols <- c("cdm_name", "target_cohort", "outcome", "competing_outcome", "variable", "result_type", "estimate_name", "estimate_value", "estimate_type")
    strata_cols <- setdiff(names(summary_result), known_cols)

    if (length(strata_cols) == 0) {
      # No stratification
      summary_result <- summary_result |>
        dplyr::mutate(
          strata_name = "overall",
          strata_level = "overall"
        )
    } else {
      # Has stratification columns - create separate rows for each strata combination
      strata_results <- list()

      # Add overall row (where all strata columns are "overall")
      overall_data <- summary_result |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(strata_cols), ~ .x == "overall"))

      if (nrow(overall_data) > 0) {
        strata_results[[1]] <- overall_data |>
          dplyr::mutate(
            strata_name = "overall",
            strata_level = "overall"
          ) |>
          dplyr::select(-dplyr::all_of(strata_cols))
      }

      # Add rows for individual strata variables (where one is not "overall", others are "overall")
      for (col in strata_cols) {
        individual_data <- summary_result |>
          dplyr::filter(.data[[col]] != "overall" & dplyr::if_all(dplyr::all_of(setdiff(strata_cols, col)), ~ .x == "overall"))

        if (nrow(individual_data) > 0) {
          strata_results[[length(strata_results) + 1]] <- individual_data |>
            dplyr::mutate(
              strata_name = col,
              strata_level = as.character(.data[[col]])
            ) |>
            dplyr::select(-dplyr::all_of(strata_cols))
        }
      }

      # Add rows for multiple strata combinations (where multiple columns are not "overall")
      multi_strata_data <- summary_result |>
        dplyr::filter(dplyr::if_any(dplyr::all_of(strata_cols), ~ .x != "overall"))

      if (nrow(multi_strata_data) > 0) {
        multi_strata_result <- multi_strata_data |>
          dplyr::rowwise() |>
          dplyr::mutate(
            strata_name = paste(strata_cols, collapse = " &&& "),
            strata_level = paste(dplyr::c_across(dplyr::all_of(strata_cols)), collapse = " &&& ")
          ) |>
          dplyr::ungroup() |>
          dplyr::select(-dplyr::all_of(strata_cols))

        strata_results[[length(strata_results) + 1]] <- multi_strata_result
      }

      summary_result <- dplyr::bind_rows(strata_results)
    }

    summary_result <- summary_result |>
      dplyr::mutate(
        group_name = "target_cohort",
        group_level = .data$target_cohort,
        variable_name = "outcome",
        variable_level = .data$variable,
        additional_name = "overall",
        additional_level = "overall"
      )

    all_results[["summary"]] <- summary_result
  }

  # Process events attribute
  if (!is.null(attr(result, "events"))) {
    events_result <- attr(result, "events") |>
      tidyr::pivot_longer(
        cols = c(dplyr::any_of(c("n_risk", "n_events", "n_censor"))),
        names_to = "estimate_name",
        values_to = "estimate_value",
        values_transform = as.character
      ) |>
      dplyr::filter(!is.na(.data$estimate_value)) |>
      dplyr::mutate(
        estimate_type = "numeric",
        estimate_name = paste0(.data$estimate_name, "_count"),
        result_type = "survival_events"
      )

    # Identify stratification columns (exclude known columns)
    known_cols <- c("cdm_name", "target_cohort", "outcome", "competing_outcome", "variable", "time", "result_type", "estimate_name", "estimate_value", "estimate_type", "eventgap")
    strata_cols <- setdiff(names(events_result), known_cols)

    if (length(strata_cols) == 0) {
      # No stratification
      events_result <- events_result |>
        dplyr::mutate(
          strata_name = "overall",
          strata_level = "overall"
        )
    } else {
      # Has stratification columns - create separate rows for each strata combination
      strata_results <- list()

      # Add overall row (where all strata columns are "overall")
      overall_data <- events_result |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(strata_cols), ~ .x == "overall"))

      if (nrow(overall_data) > 0) {
        strata_results[[1]] <- overall_data |>
          dplyr::mutate(
            strata_name = "overall",
            strata_level = "overall"
          ) |>
          dplyr::select(-dplyr::all_of(strata_cols))
      }

      # Add rows for individual strata variables (where one is not "overall", others are "overall")
      for (col in strata_cols) {
        individual_data <- events_result |>
          dplyr::filter(.data[[col]] != "overall" & dplyr::if_all(dplyr::all_of(setdiff(strata_cols, col)), ~ .x == "overall"))

        if (nrow(individual_data) > 0) {
          strata_results[[length(strata_results) + 1]] <- individual_data |>
            dplyr::mutate(
              strata_name = col,
              strata_level = as.character(.data[[col]])
            ) |>
            dplyr::select(-dplyr::all_of(strata_cols))
        }
      }

      # Add rows for multiple strata combinations (where multiple columns are not "overall")
      multi_strata_data <- events_result |>
        dplyr::filter(dplyr::if_any(dplyr::all_of(strata_cols), ~ .x != "overall"))

      if (nrow(multi_strata_data) > 0) {
        multi_strata_result <- multi_strata_data |>
          dplyr::rowwise() |>
          dplyr::mutate(
            strata_name = paste(strata_cols, collapse = " &&& "),
            strata_level = paste(dplyr::c_across(dplyr::all_of(strata_cols)), collapse = " &&& ")
          ) |>
          dplyr::ungroup() |>
          dplyr::select(-dplyr::all_of(strata_cols))

        strata_results[[length(strata_results) + 1]] <- multi_strata_result
      }

      events_result <- dplyr::bind_rows(strata_results)
    }

    events_result <- events_result |>
      dplyr::mutate(
        group_name = "target_cohort",
        group_level = .data$target_cohort,
        variable_name = "outcome",
        variable_level = .data$variable,
        additional_name = "time",
        additional_level = as.character(.data$time)
      )

    all_results[["events"]] <- events_result
  }

  # Process attrition attribute
  if (!is.null(attr(result, "attrition"))) {
    attrition_result <- attr(result, "attrition") |>
      # Attrition data is already in long format with variable_name containing the estimate names
      dplyr::mutate(
        estimate_name = "count",
        estimate_type = "integer",
        result_type = "survival_attrition",
        estimate_value = as.character(.data$count)
      ) |>
      dplyr::mutate(
        group_name = "target_cohort",
        group_level = .data$target_cohort,
        strata_name = "reason",
        strata_level = .data$reason,
        variable_level = .data$outcome,
        additional_name = "reason_id",
        additional_level = dplyr::if_else(
          grepl("Initial", .data$reason), "1",
                dplyr::if_else(
                  grepl("washout", .data$reason), "2",
                  dplyr::if_else(
                    grepl("for outcome", .data$reason), "3", "4"))
        )
      )

    all_results[["attrition"]] <- attrition_result
  }

  # Combine all results
  if (length(all_results) == 0) {
    cli::cli_abort("No valid data found in survival_result object.")
  }

  final_result <- dplyr::bind_rows(all_results)

  # Now assign result_id values by joining with the original settings
  # Create a lookup table from the original settings
  result_id_lookup <- original_settings |>
    dplyr::select("result_id", "result_type",
                 dplyr::any_of(c("outcome", "competing_outcome", "eventgap",
                                "analysis_type", "package_name", "package_version")))

  # Join the final result with the lookup to get correct result_ids
  final_result <- final_result |>
    dplyr::left_join(result_id_lookup, by = "result_type") |>
    # Handle cases where the join didn't work (create fallback IDs)
    dplyr::mutate(result_id = dplyr::coalesce(.data$result_id, dplyr::row_number()))

  # Ensure we have all required columns
  required_cols <- omopgenerics::resultColumns()
  missing_cols <- setdiff(required_cols, colnames(final_result))

  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      final_result <- final_result |>
        dplyr::mutate(!!col := NA_character_)
    }
  }

  # Select only required columns and unite
  final_result <- final_result |>
    dplyr::select(dplyr::all_of(required_cols))

  # Use the original settings directly
  final_result <- omopgenerics::newSummarisedResult(final_result, settings = original_settings)

  return(final_result)
}
