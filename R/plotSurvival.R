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
#' @param x Variable to plot on x axis
#' @param xscale X axis scale. Can be "days" or "years".
#' @param ylim Limits for the Y axis
#' @param xlim Limits for the X axis
#' @param cumulativeFailure whether to plot the cumulative failure probability
#' instead of the survival probability
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param riskTable Whether to print risk table below the plot
#' @param riskInterval Interval of time to print risk table below the plot
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
                         x = "time",
                         xscale = "days",
                         ylim = c(0, NA),
                         xlim = NULL,
                         cumulativeFailure = FALSE,
                         ribbon = TRUE,
                         facet = NULL,
                         colour = NULL,
                         riskTable = FALSE,
                         riskInterval = 30) {

  # Missing input checks
   omopgenerics::assertNumeric(xlim, min = 1, length = 1, integerish = TRUE, null = TRUE)
  if(is.null(xlim)) {xlim <- "Inf"}

  result <- result %>%
    asSurvivalResult() %>%
    dplyr::filter(.data$time <= xlim) %>%
    dplyr::compute()

  if (isFALSE(cumulativeFailure) && "cumulative_failure_probability" %in% unique(result$result_type)) {
    cli::cli_abort("cumulativeFailure must be TRUE if result comes from a competing risk analysis")
  }

  if (cumulativeFailure) {
    result <- result %>%
      dplyr::mutate(
        estimate_value = dplyr::if_else(.data$result_type == "cumulative_failure_probability",
                                        .data$estimate_value,
                                        1 - .data$estimate_value)
      )
    plot_name <- "Cumulative failure probability"
  } else {
    plot_name <- "Survival probability"
  }

  plot <- plotEstimates(result,
                        x = x,
                        xscale = xscale,
                        y = "estimate",
                        yLower = "estimate_95CI_lower",
                        yUpper = "estimate_95CI_upper",
                        ylim = ylim,
                        ytype = "count",
                        ribbon = ribbon,
                        facet = facet,
                        colour = colour) +
    ggplot2::ylab(plot_name)

  if (xscale == "years") {
    plot <- plot +
      ggplot2::xlab("Time in years") +
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  } else {
    plot <- plot +
      ggplot2::xlab("Time in days")
  }

  if (riskTable) {
    max_t <- result %>%
      dplyr::pull(.data$time) %>%
      max()

    riskTimes <- seq(0, max_t, by = riskInterval)

    if (!is.null(facet)) {
      result <- result %>%
        dplyr::mutate(!!facet := gsub("&&&","and",result[[facet]]))
      attr(result, "events") <- attr(result, "events") %>%
        dplyr::mutate(!!facet := gsub("&&&","and",attr(result, "events")[[facet]]))

      facetLevels <- unique(result[[facet]])

      plotList <- list()

      applyPlot <- function(plotList, level) {
        subResult <- result %>%
          dplyr::filter(!!rlang::sym(facet) == level) %>%
          dplyr::compute()
        attr(subResult, "events") <- attr(result, "events") %>%
          dplyr::filter(!!rlang::sym(facet) == level) %>%
          dplyr::compute()

        facetPlot <- plotEstimates(subResult,
                                   x = x,
                                   xscale = xscale,
                                   y = "estimate",
                                   yLower = "estimate_95CI_lower",
                                   yUpper = "estimate_95CI_upper",
                                   ylim = ylim,
                                   ytype = "count",
                                   ribbon = ribbon,
                                   facet = NULL,
                                   colour = colour) +
          ggplot2::ggtitle(level)

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
                 tidyr::pivot_longer(c(names_risk, .data$time), names_to = "layer", values_to = "label") %>%
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
        tidyr::pivot_longer(c(names_risk, .data$time), names_to = "layer", values_to = "label") %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$timeb)) +
        ggplot2::geom_text(ggplot2::aes(y = factor(.data$layer, c(names_risk, "time")), label = dplyr::if_else(is.na(.data$label), sprintf("NA"), .data$label))) +
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
      dplyr::filter(.data$estimate_name == "n_risk",
                    .data$time %in% riskTimes) %>%
      dplyr::mutate(n_risk = as.character(.data$estimate_value)) %>%
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
        dplyr::mutate(!!i := as.character(NA))
    }

    riskdata <- dplyr::union_all(riskdata, riskdataend) %>%
      dplyr::mutate(n_risk = dplyr::if_else(is.na(.data$n_risk), "", .data$n_risk))
  } else {
    riskdata <- attr(result, "events") %>%
      dplyr::filter(.data$estimate_name == "n_risk",
                    .data$time %in% riskTimes) %>%
      dplyr::mutate(n_risk = as.character(.data$estimate_value)) %>%
      dplyr::select("time", "n_risk", colour) %>%
      dplyr::mutate(!!colour := stringr::str_replace_all(.data[[colour]], "&&&", "and")) %>%
      dplyr::mutate(timeb = .data$time,
                    time = as.character(.data$time),
                    !!colour := paste0("n_risk_", .data[[colour]])) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = colour, values_from = .data$n_risk)

    riskdataend <- dplyr::tibble(
      time = as.character(riskTimes),
      timeb = riskTimes
    ) %>%
      dplyr::filter(!(.data$time %in% (riskdata %>% dplyr::pull("time"))))

    for (i in colnames(riskdata %>% dplyr::select(dplyr::starts_with("n_risk")))) {
      riskdataend <- riskdataend %>%
        dplyr::mutate(!!i := as.character(NA))
    }

    riskdata <- dplyr::union_all(riskdata, riskdataend) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, "")))

    names(riskdata) <- gsub("n risk ", "", gsub("_", " ", names(riskdata)))
  }
  return(riskdata)
}

# helper functions
plotEstimates <- function(result,
                          x,
                          xscale,
                          y,
                          yLower,
                          yUpper,
                          ylim,
                          ytype,
                          ribbon,
                          facet,
                          colour){

  omopgenerics::assertCharacter(xscale, length = 1)
  omopgenerics::assertChoice(xscale, c("days", "years"))
  omopgenerics::assertChoice(c(x), colnames(result))

  plot_data <- getPlotData(estimates = result,
                           facetVars = facet,
                           colourVars = colour)

  if(xscale == "years"){
    plot_data <- plot_data %>%
      dplyr::mutate(time = .data$time / 365.25)
  }

  if(is.null(colour)){
    plot <- plot_data %>%
      ggplot2::ggplot(
        ggplot2::aes(x = !!rlang::sym(x),
                     y = !!rlang::sym(y)))
  } else {
    plot <- plot_data %>%
      ggplot2::ggplot(
        ggplot2::aes(x = !!rlang::sym(x) ,
                     y = !!rlang::sym(y),
                     group = .data$colour_vars,
                     colour = .data$colour_vars,
                     fill = .data$colour_vars,
                     linetype = .data$colour_vars)) +
      ggplot2::labs(colour  = "legend",
                    linetype = "legend")
  }

  plot <- plot +
    ggplot2::geom_line(linewidth = 0.25)
  if(is.null(ylim)){
    if(ytype == "count"){
      plot <- plot +
        ggplot2::scale_y_continuous(labels = scales::comma)
    }
    if(ytype == "percentage"){
      plot <- plot +
        ggplot2::scale_y_continuous(labels =
                                      scales::percent_format(accuracy = 0.1))
    }
  } else {
    plot <- addYLimits(plot = plot, ylim = ylim, ytype = ytype)
  }

  if(!is.null(facet)){
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data$facet_var)) +
      ggplot2::theme_bw()
  } else {
    plot <- plot +
      ggplot2::theme_minimal()
  }

  if(isTRUE(ribbon)){
    plot <- addRibbon(plot = plot, yLower = yLower, yUpper = yUpper)
  }



  plot <- plot +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  return(plot)

}

getPlotData <- function(estimates, facetVars, colourVars){

  plotData <- estimates %>%
    tidyr::pivot_wider(names_from = "estimate_name",
                       values_from = "estimate_value")

  if(!is.null(facetVars)){
    plotData <- plotData %>%
      tidyr::unite("facet_var",
                   c(dplyr::all_of(.env$facetVars)), remove = FALSE,
                   sep = "; ") |>
      dplyr::mutate(facet_var = stringr::str_replace_all(
        .data$facet_var, "&&&", "and"
      ))

  }
  if(!is.null(colourVars)){
    plotData <- plotData %>%
      tidyr::unite("colour_vars",
                   c(dplyr::all_of(.env$colourVars)), remove = FALSE,
                   sep = "; ") |>
      dplyr::mutate(colour_vars = stringr::str_replace_all(
        .data$colour_vars, "&&&", "and"
      ))

  }

  return(plotData)

}

addYLimits <- function(plot, ylim, ytype){
  if(ytype == "count"){
    plot <- plot +
      ggplot2::scale_y_continuous(labels = scales::comma,
                                  limits = ylim)
  }
  if(ytype == "percentage"){
    plot <- plot +
      ggplot2::scale_y_continuous(labels =
                                    scales::percent_format(accuracy = 0.1),
                                  limits = ylim)
  }
  return(plot)
}

addRibbon <- function(plot, yLower, yUpper){
  plot <- plot  +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = !!rlang::sym(yLower),
                   ymax = !!rlang::sym(yUpper)),
      alpha = .3, color = NA, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 0.25)
}
