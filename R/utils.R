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

#'  Participants contributing to a survival analysis
#'
#' @param result Result object
#'
#' @return References to the study participants contributing to
#' a given analysis
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(cdm,
#'                                     targetCohortTable = "mgus_diagnosis",
#'                                     outcomeCohortTable = "death_cohort",
#'                                     returnParticipants = TRUE)
#' survivalParticipants(surv)
#'}
survivalParticipants <- function(result) {
  attr(result, "participants")
}

getColumns <- function(result, col, overall) {
  # initial checks
  checkmate::assertTibble(result)
  checkmate::assertCharacter(col, any.missing = FALSE, len = 1)
  checkmate::assertTRUE(col %in% colnames(result))
  checkmate::assertLogical(overall, any.missing = FALSE, len = 1)

  # extract columns
  x <- result %>%
    dplyr::pull(dplyr::all_of(col)) %>%
    unique() %>%
    lapply(strsplit, split = " and ") %>%
    unlist() %>%
    unique()

  # eliminate overall
  if (!overall) {
    x <- x[x != "overall"]
  }

  return(x)
}
