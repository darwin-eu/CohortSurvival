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

checkExposureCohortId <- function(cohort) {
  isCohortIdUnique <- length(cohort |>
                          dplyr::select("cohort_definition_id") |>
                          dplyr::pull() |>
                          unique()) == 1

  if(isFALSE(isCohortIdUnique)) {
    return(cli::cli_abort(c(
      "the exposure cohort must only have one id in cohort_definition_id in addSurvival stage"
    )))
  }
}

checkCensorOnDate <- function(cohort, censorOnDate) {
  if (!is.null(censorOnDate)) {
    # allow either a Date or the name of a column on the cohort that contains per-subject censor dates
    if (is.character(censorOnDate)) {
      omopgenerics::assertCharacter(censorOnDate, length = 1)
      if (!(censorOnDate %in% colnames(cohort))) {
        return(cli::cli_abort(c("the cohort does not contain a column named '{censorOnDate}'")))
      }
      censor_dates <- cohort |>
        dplyr::select(dplyr::all_of(censorOnDate)) |>
        dplyr::pull()
      if (!inherits(censor_dates, "Date")) {
        return(cli::cli_abort(c(
          "the column '{censorOnDate}' must be of Date type"
        )))
      }
      start_dates <- cohort |>
        dplyr::select("cohort_start_date") |>
        dplyr::pull()
      if (max(start_dates, na.rm = TRUE) > max(censor_dates, na.rm = TRUE)) {
        return(cli::cli_abort(c(
          "the target cohort has at least one cohort_start_date after the maximum censor date in column {censorOnDate}"
        )))
      }
    } else {
      start_dates <- cohort |>
        dplyr::select("cohort_start_date") |>
        dplyr::pull()
      if (max(start_dates) > censorOnDate) {
        return(cli::cli_abort(c(
          "the target cohort has at least one cohort_start_date after the censor date {censorOnDate}"
        )))
      }
    }
  }
}
