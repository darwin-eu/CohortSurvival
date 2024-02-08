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

#' Unite one or more columns in name-level format.
#'
#' @param x Tibble or data.frame.
#' @param cols Columns to aggregate.
#' @param name Column name of the `name` column.
#' @param level Column name of the `level` column.
#' @param keep Whether to keep the original columns.
#'
#' @return A Tibble with the new columns.
#'
#' @examples
#' \donttest{
#' x <- dplyr::tibble(
#'   variable = "number subjects",
#'   value = c(10, 15, 40, 78),
#'   sex = c("Male", "Female", "Male", "Female"),
#'   age_group = c("<40", ">40", ">40", "<40")
#' )
#'
#' x
#'
#' x |>
#'   CohortSurvival:::uniteNameLevel(
#'     cols = c("sex", "age_group"),
#'     name = "new_column_name",
#'     level = "new_column_level"
#'   )
#' }
#'
#'
uniteNameLevel <- function(x,
                           cols,
                           name = "group_name",
                           level = "group_level",
                           keep = FALSE) {
  # initial checks
  checkmate::assertCharacter(cols)
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(level, len = 1, any.missing = FALSE)
  checkmate::assertLogical(keep, len = 1, any.missing = FALSE)
  checkmate::assertTibble(x)
  checkmate::assertTRUE(all(cols %in% colnames(x)))

  id <- min(which(colnames(x) %in% cols))

  present <- c(name, level)[c(name, level) %in% colnames(x)]
  if (length(present) > 0) {
    cli::cli_warn(
      "The following columns will be overwritten:
      {paste0(present, collapse = ', ')}."
    )
  }

  containAnd <- cols[grepl(" and ", cols)]
  if (length(containAnd) > 0) {
    cli::cli_abort("Column names must not contain ' and ' : `{paste0(containAnd, collapse = '`, `')}`")
  }
  containAnd <- cols[
    lapply(cols, function(col){any(grepl(" and ", x[[col]]))}) |> unlist()
  ]
  if (length(containAnd) > 0) {
    cli::cli_abort("Column values must not contain ' and '. Present in: `{paste0(containAnd, collapse = '`, `')}`.")
  }

  x <- x |>
    dplyr::mutate(!!name := paste0(cols, collapse = " and ")) |>
    tidyr::unite(
      col = !!level, dplyr::all_of(cols), sep = " and ", remove = !keep
    )

  if (keep) {
    colskeep <- cols
  } else {
    colskeep <- character()
  }

  # move cols
  if (id == 1) {
    x <- x |> dplyr::relocate(dplyr::all_of(c(colskeep, name, level)))
  } else {
    id <- colnames(x)[id - 1]
    x <- x |>
      dplyr::relocate(
        dplyr::all_of(c(colskeep, name, level)), .after = dplyr::all_of(id)
      )
  }

  return(x)
}

#' Split name and level columns into the columns.
#'
#' @param result Omop result object (summarised_result or compared_result).
#' @param name Column with the names.
#' @param level Column with the levels.
#' @param keep Whether to keep the original group_name and group_level columns.
#' @param overall Whether to keep overall column if present.
#'
#' @return A dataframe with the specified name column values as columns.
#' @description
#' Pivots the input dataframe so the values of the name columns are tranformed
#' into columns, which values come from the specified level column.
#'
#' @export
#'
#' @examples
#' x <- dplyr::tibble(
#'   variable = "number subjects",
#'   value = c(10, 15, 40, 78),
#'   group_name = c("sex and age_group", "sex and age_group",
#'   "sex and age_group", "sex and age_group"),
#'   group_level = c("Male and <40", "Female and >40", "Male and >40", "Male and <40")
#' )
#'   x |> splitNameLevel(name = "group_name",
#'                  level = "group_level",
#'                  keep = FALSE)
#'
splitNameLevel <- function(result,
                           name = "group_name",
                           level = "group_level",
                           keep = FALSE,
                           overall = FALSE) {
  checkmate::assertCharacter(name, len = 1, any.missing = FALSE)
  checkmate::assertCharacter(level, len = 1, any.missing = FALSE)
  checkmate::assertLogical(keep, len = 1, any.missing = FALSE)
  checkmate::assertTibble(result)
  checkmate::assertTRUE(all(c(name, level) %in% colnames(result)))

  newCols <- getColumns(result, name, TRUE)
  id <- which(name == colnames(result))

  nameValues <- result[[name]] |> stringr::str_split(" and ")
  levelValues <- result[[level]] |> stringr::str_split(" and ")
  if (!all(lengths(nameValues) == lengths(levelValues))) {
    cli::cli_abort("Column names and levels number does not match")
  }

  nameValue <- unique(unlist(nameValues))
  present <- nameValue[nameValue %in% colnames(result)]
  if (length(present) > 0) {
    cli::cli_warn(
      "The following columns will be overwritten:
      {paste0(present, collapse = ', ')}."
    )
  }
  for (k in seq_along(nameValue)) {
    col <- nameValue[k]
    dat <- lapply(seq_along(nameValues), function(y) {
      res <- levelValues[[y]][nameValues[[y]] == col]
      if (length(res) == 0) {
        return(as.character(NA))
      } else {
        return(res)
      }
    }) |>
      unlist()
    result[[col]] <- dat
  }

  if (!keep) {
    result <- result |> dplyr::select(-dplyr::all_of(c(name, level)))
    colskeep <- character()
  } else {
    colskeep <- c(name, level)
  }

  if ("overall" %in% newCols & !overall) {
    result <- result |> dplyr::select(-"overall")
  }

  # move cols
  if (id == 1) {
    result <- result |> dplyr::relocate(dplyr::any_of(newCols))
  } else {
    id <- colnames(result)[id - 1]
    result <- result |>
      dplyr::relocate(
        dplyr::any_of(c(colskeep, newCols)), .after = dplyr::all_of(id)
      )
  }

  return(result)
}

getColumns <- function(result, col, overall) {
  # initial checks
  checkmate::assertTibble(result)
  checkmate::assertCharacter(col, any.missing = FALSE, len = 1)
  checkmate::assertTRUE(col %in% colnames(result))
  checkmate::assertLogical(overall, any.missing = FALSE, len = 1)

  # extract columns
  x <- result |>
    dplyr::pull(dplyr::all_of(col)) |>
    unique() |>
    lapply(strsplit, split = " and ") |>
    unlist() |>
    unique()

  # eliminate overall
  if (!overall) {
    x <- x[x != "overall"]
  }

  return(x)
}
