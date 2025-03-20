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

checkCohortId <- function(cohort, cohortId) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(cohortId,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  nrow(omopgenerics::settings(cohort) %>%
    dplyr::filter(.data$cohort_definition_id %in% .env$cohortId)) == length(cohortId)
}

checkExposureCohortId <- function(cohort) {
  isCohortIdUnique <- length(cohort %>%
                          dplyr::select("cohort_definition_id") %>%
                          dplyr::pull() %>%
                          unique()) == 1

  if(isFALSE(isCohortIdUnique)) {
    return(cli::cli_abort(c(
      "the exposure cohort must only have one id in cohort_definition_id in addSurvival stage"
    )))
  }
}

checkCensorOnDate <- function(cohort, censorOnDate) {
  if(!is.null(censorOnDate)) {
    start_dates <- cohort %>%
      dplyr::select("cohort_start_date") %>%
      dplyr::pull()
    if(max(start_dates) > censorOnDate) {
      return(cli::cli_abort(c(
        "the target cohort has at least one cohort_start_date after the censor date {censorOnDate}"
      )))
    }
  }
}
