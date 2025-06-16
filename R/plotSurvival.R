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

#' Plot survival results
#'
#' @param result Survival results
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param cumulativeFailure whether to plot the cumulative failure probability
#' instead of the survival probability
#' @param riskTable Whether to print risk table below the plot
#' @param riskInterval Interval of time to print risk table below the plot
#' @param logLog If TRUE, the survival probabilities are transformed using the log-log formula
#' @param timeScale The scale of time in the x-axis. Can be "days", "months", or "years"
#'
#' @return A plot of survival probabilities over time
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(cdm,
#'                                     targetCohortTable = "mgus_diagnosis",
#'                                     outcomeCohortTable = "death_cohort")
#' plotSurvival(surv)
#'}
#'
plotSurvival <- function(result,
                         ribbon = TRUE,
                         facet = NULL,
                         colour = NULL,
                         cumulativeFailure = FALSE,
                         riskTable = FALSE,
                         riskInterval = 30,
                         logLog = FALSE,
                         timeScale = "days") {

  rlang::check_installed("visOmopResults")
  rlang::check_installed("ggplot2")
  rlang::check_installed("scales")

  # Missing input checks
  if (nrow(result) == 0) {
    cli::cli_warn("Empty result object")
    return(visOmopResults::emptyPlot())
  }

  result <- result %>% asSurvivalResult()

  # Adjust time scale
  if (timeScale == "years") {
    result <- result %>%
      dplyr::mutate(time = .data$time / 365.25)
    xlab <- "Time in years"
  } else if (timeScale == "months") {
    result <- result %>%
      dplyr::mutate(time = .data$time / 30.4375)
    xlab <- "Time in months"
  } else {
    xlab <- "Time in days"
  }

  if (isFALSE(cumulativeFailure) && "cumulative_failure_probability" %in% unique(result$result_type)) {
    cli::cli_abort("cumulativeFailure must be TRUE if result comes from a competing risk analysis")
  }

  if (cumulativeFailure & unique(result$result_type) != "cumulative_failure_probability") {
    result <- result %>%
      dplyr::mutate(
        result_type = "cumulative_failure_probability",
        estimate = 1 - .data$estimate,
        estimate_95CI_lower = 1 - .data$estimate_95CI_lower,
        estimate_95CI_upper = 1 - .data$estimate_95CI_upper
      )
  }

  # Only plot resuts for which all estimates are not NA
  result <- result %>%
    dplyr::filter(
      !is.na(.data$estimate),
      !is.na(.data$estimate_95CI_lower),
      !is.na(.data$estimate_95CI_upper)
    )

  plot_name <- stringr::str_to_sentence(gsub("_", " ", unique(result$result_type)))

  if (logLog) {
    result <- result %>%
      dplyr::mutate(estimate = log(-log(.data$estimate)),
                    estimate_95CI_lower = log(-log(.data$estimate_95CI_lower)),
                    estimate_95CI_upper = log(-log(.data$estimate_95CI_upper)),
                    time = log(.data$time))

    plot_name <- paste0("Log-log ",plot_name)
  }

  labels <- c("estimate", "estimate_95CI_lower", "estimate_95CI_upper", "target_cohort", "outcome", "competing_outcome")

  plot <- visOmopResults::scatterPlot(
    result = result,
    x = "time",
    y = "estimate",
    line = TRUE,
    point = FALSE,
    ribbon = FALSE,
    ymin = NULL,
    ymax = NULL,
    facet = facet,
    colour = colour,
    label = labels
  ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(plot_name) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    visOmopResults::themeVisOmop()

  if (ribbon) {
    plot <- plot %>% addRibbon()
  }

  if (riskTable) {
    max_t <- result %>%
      dplyr::pull(.data$time) %>%
      max()

    riskTimes <- seq(0, max_t, by = riskInterval)

    if (!is.null(facet)) {
      x <- result %>%
        tidyr::unite("facet", dplyr::any_of(facet), sep = " and ", remove = FALSE)

      attr(x, "events") <- attr(result, "events") %>%
        tidyr::unite("facet", dplyr::any_of(facet), sep = " and ", remove = FALSE)

      facetLevels <- unique(x[["facet"]])

      plotList <- list()

      applyPlot <- function(plotList, level) {
        subResult <- x %>%
          dplyr::filter(.data$facet == level)
        attr(subResult, "events") <- attr(x, "events") %>%
          dplyr::filter(.data$facet == level) %>%
          dplyr::compute()

        facetPlot <- visOmopResults::scatterPlot(
          result = subResult,
          x = "time",
          y = "estimate",
          line = TRUE,
          point = FALSE,
          ribbon = FALSE,
          ymin = NULL,
          ymax = NULL,
          facet = NULL,
          colour = colour,
          label = character()
        ) +
          ggplot2::xlab("Time in days") +
          ggplot2::ylab(plot_name) +
          ggplot2::scale_y_continuous(labels = scales::comma) +
          ggplot2::ggtitle(level) +
          visOmopResults::themeVisOmop()

        if (ribbon) {
          facetPlot <- facetPlot %>% addRibbon()
        }

        riskData <- generateRiskData(subResult, riskTimes, colour)

        if (nrow(riskData) == 0) {
          cli::cli_abort("Check the riskInterval provided. It seems that interval
                        does not provide the times for which n_risk can be retrieved.
                        Check the `events` attribute from your asSurvivalResult()
                        object to know the times at which `n_risk` information is
                        available")
        }

        names_risk <- riskData %>%
          dplyr::select(-c(dplyr::starts_with("time"))) %>%
          colnames()

        nameRisk <- paste0("p",as.character(level))
        assign(nameRisk, riskData %>%
                 tidyr::pivot_longer(dplyr::all_of(c(names_risk, "time")), names_to = "layer", values_to = "label") %>%
                 ggplot2::ggplot(ggplot2::aes(x = .data$timeb)) +
                 ggplot2::geom_text(ggplot2::aes(y = factor(.data$layer, c(names_risk, "time")), label = dplyr::if_else(is.na(.data$label), sprintf("NA"), .data$label))) +
                 ggplot2::labs(y = "", x = NULL) +
                 ggplot2::theme_minimal() +
                 ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                                panel.grid = ggplot2::element_blank(), strip.text = ggplot2::element_blank())
        )

        plotList[[as.character(level)]] <- facetPlot / patchwork::wrap_elements(get(nameRisk)) + patchwork::plot_layout(heights = c(8, 1))
        return(plotList)
      }

      plotList <- purrr::reduce(facetLevels, applyPlot, .init = plotList)

      finalPlot <- patchwork::wrap_plots(plotList)
      return(finalPlot)
    } else {

      riskData <- generateRiskData(result, riskTimes, colour)

      if (nrow(riskData) == 0) {
        cli::cli_abort("Check the riskInterval provided. It seems that interval
                      does not provide the times for which n_risk can be retrieved.
                      Check the `events` attribute from your asSurvivalResult()
                      object to know the times at which `n_risk` information is
                      available")
      }

      names_risk <- riskData %>%
        dplyr::select(-c(dplyr::starts_with("time"))) %>%
        colnames()

      p2 <- riskData %>%
        tidyr::pivot_longer(dplyr::all_of(c(names_risk, "time")), names_to = "layer", values_to = "label") %>%
        dplyr::mutate(layer = visOmopResults::customiseText(.data$layer)) %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$timeb)) +
        ggplot2::geom_text(ggplot2::aes(y = factor(.data$layer, visOmopResults::customiseText(c(names_risk, "time"))), label = dplyr::if_else(is.na(.data$label), sprintf("NA"), .data$label))) +
        ggplot2::labs(y = "", x = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(), strip.text = ggplot2::element_blank())

      plot <- plot / patchwork::wrap_elements(p2) + patchwork::plot_layout(heights = c(8, 1))
    }
  }

  return(plot)
}

generateRiskData <- function(result, riskTimes, colour) {
  if (is.null(colour)) {
    riskdata <- attr(result, "events") %>%
      dplyr::filter(.data$time %in% riskTimes) %>%
      dplyr::mutate(n_risk = as.character(.data$n_risk)) %>%
      dplyr::select("time", "n_risk") %>%
      dplyr::mutate(timeb = .data$time,
                    time = as.character(.data$time))

    riskdataend <- dplyr::tibble(
      time = as.character(riskTimes),
      timeb = riskTimes
    ) %>%
      dplyr::filter(!(.data$time %in% (riskdata %>% dplyr::pull("time"))))

    for (i in colnames(riskdata %>% dplyr::select(dplyr::starts_with("n_risk")))) {
      riskdataend <- riskdataend %>%
        dplyr::mutate(!!i := NA_character_)
    }

    riskdata <- dplyr::union_all(riskdata, riskdataend) %>%
      dplyr::mutate(n_risk = dplyr::if_else(is.na(.data$n_risk), "", .data$n_risk))

  } else {
    riskdata <- attr(result, "events") %>%
      dplyr::filter(.data$time %in% riskTimes) %>%
      dplyr::mutate(n_risk = as.character(.data$n_risk)) %>%
      tidyr::unite("colour", colour, sep = " and ", remove = FALSE) %>%
      dplyr::select("time", "n_risk", "colour") %>%
      dplyr::mutate(colour := stringr::str_replace_all(.data$colour, "&&&", "and")) %>%
      dplyr::mutate(timeb = .data$time,
                    time = as.character(.data$time),
                    colour := paste0("n_risk", .data$colour)) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = dplyr::any_of("colour"), values_from = .data$n_risk)

    riskdataend <- dplyr::tibble(
      time = as.character(riskTimes),
      timeb = riskTimes
    ) %>%
      dplyr::filter(!(.data$time %in% (riskdata %>% dplyr::pull("time"))))

    for (i in colnames(riskdata %>% dplyr::select(dplyr::starts_with("n_risk")))) {
      riskdataend <- riskdataend %>%
        dplyr::mutate(!!i := NA_character_)
    }

    riskdata <- dplyr::union_all(riskdata, riskdataend) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, "")))
  }
  colnames(riskdata) <- gsub("n risk", "", gsub("_", " ", colnames(riskdata)))
  colnames(riskdata)[colnames(riskdata) == rep("overall", length(colour)) %>% paste0(collapse = " and ")] <- "overall"
  return(riskdata)
}

addRibbon <- function(plot){
  plot <- plot  +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$estimate_95CI_lower,
                   ymax = .data$estimate_95CI_upper),
      alpha = 0.3, color = NA, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 0.25)
}
