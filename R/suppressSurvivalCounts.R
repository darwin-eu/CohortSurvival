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
      as.character(estimate))) %>%
    dplyr::mutate(estimate = dplyr::if_else(
      .data$group_name == toObscure$group_name[i] &
        .data$group_level == toObscure$group_level[i] &
        .data$strata_name == toObscure$strata_name[i]  &
        .data$strata_level == toObscure$strata_level[i]  &
        .data$variable_type != "n_start",
      as.character(NA),
      as.character(estimate)))
}
  }


  return(result)
}

