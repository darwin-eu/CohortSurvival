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

suppressSurvivalCounts <- function(result,
                           minCellCount = 5) {

  checkmate::assertTRUE(all(c(
    "variable", "estimate", "estimate_type", "group_name", "group_level",
    "strata_name", "strata_level"
  ) %in%
    colnames(result)))

  checkmate::assertIntegerish(minCellCount,
                              len = 1,
                              lower = 0)

  if (minCellCount > 1) {
    toObscure <- result %>%
      dplyr::filter(.data$variable_type == "n_start") %>%
      dplyr::mutate(estimate = as.integer(.data$estimate)) %>%
      dplyr::filter(.data$estimate > 0 & .data$estimate < .env$minCellCount) %>%
      dplyr::select("group_name", "group_level", "strata_name", "strata_level")


for(i in seq_along(toObscure$group_name)){
  result <- result %>%
    dplyr::mutate(estimate = dplyr::if_else(
      .data$group_name == toObscure$group_name[i] &
      .data$group_level == toObscure$group_level[i] &
      .data$strata_name == toObscure$strata_name[i]  &
      .data$strata_level == toObscure$strata_level[i]  &
      .data$variable_type == "n_start", paste0("<", minCellCount),
      as.character(.data$estimate))) %>%
    dplyr::mutate(estimate = dplyr::if_else(
      .data$group_name == toObscure$group_name[i] &
        .data$group_level == toObscure$group_level[i] &
        .data$strata_name == toObscure$strata_name[i]  &
        .data$strata_level == toObscure$strata_level[i]  &
        .data$variable_type != "n_start",
      as.character(NA),
      as.character(.data$estimate)))
}
  }


  return(result)
}

