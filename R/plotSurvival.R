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
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param colour_name Colour legend name
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
#' plot <- plotSurvival(surv)
#' plot
#'}
#'
plotSurvival <- function(result,
                         x = "time",
                         xscale = "days",
                         ylim = c(0,NA),
                         ribbon = TRUE,
                         facet = NULL,
                         colour = NULL,
                         colour_name = NULL){


 plot <- plotEstimates(result = result %>%
                  dplyr::filter(.data$estimate_type ==
                                  "Survival probability"),
                x = x,
                xscale = xscale,
                y = "estimate",
                yLower = "estimate_95CI_lower",
                yUpper = "estimate_95CI_upper",
                ylim = ylim,
                ytype = "count",
                ribbon = ribbon,
                facet = facet,
                colour = colour,
                colour_name = colour_name) +
    ggplot2::ylab("Survival probability")

  if(xscale == "years"){
    plot <- plot+
      ggplot2::xlab("Time in years")+
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  } else {
    plot <- plot+
      ggplot2::xlab("Time in days")
  }

 return(plot)

}

#' Plot cumulative incidence
#'
#' @param result Survival results
#' @param x Variable to plot on x axis
#' @param xscale X axis scale. Can be "days" or "years".
#' @param ylim Limits for the Y axis
#' @param ribbon If TRUE, the plot will join points using a ribbon
#' @param facet Variables to use for facets
#' @param colour Variables to use for colours
#' @param colour_name Colour legend name
#'
#' @return A plot of cumulative incidence over time
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockMGUS2cdm()
#' surv <- estimateSingleEventSurvival(cdm,
#'                                     targetCohortTable = "mgus_diagnosis",
#'                                     outcomeCohortTable = "death_cohort")
#' plot <- plotCumulativeIncidence(surv)
#' plot
#'}
plotCumulativeIncidence <- function(result,
                                    x = "time",
                                    xscale = "days",
                                    ylim = c(0,NA),
                                    ribbon = TRUE,
                                    facet = NULL,
                                    colour = NULL,
                                    colour_name = NULL){


  plot <- plotEstimates(result = result %>%
                  dplyr::filter(.data$estimate_type ==
                                  "Cumulative failure probability"),
                x = x,
                xscale = xscale,
                y = "estimate",
                yLower = "estimate_95CI_lower",
                yUpper = "estimate_95CI_upper",
                ylim = ylim,
                ytype = "count",
                ribbon = ribbon,
                facet = facet,
                colour = colour,
                colour_name = colour_name) +
    ggplot2::ylab("Cumulative failure probability")

  if(xscale == "years"){
    plot <- plot+
      ggplot2::xlab("Time in years")+
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  } else {
    plot <- plot+
      ggplot2::xlab("Time in days")
  }

  return(plot)


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
                          colour,
                          colour_name){

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_character(xscale, len = 1)
  checkmate::assertTRUE(xscale %in% c("days", "years"))
  #checkmate::assertTRUE(inherits(result, "SurvivalResult"))
  checkmate::assertTRUE(all(c(x) %in% colnames(result)))
  checkmate::reportAssertions(collection = errorMessage)

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
    tidyr::pivot_wider(names_from = "variable_type",
                       values_from = "estimate")

  if(!is.null(facetVars)){
    plotData <- plotData %>%
      tidyr::unite("facet_var",
                   c(dplyr::all_of(.env$facetVars)), remove = FALSE, sep = "; ")
  }
  if(!is.null(colourVars)){
    plotData <-plotData %>%
      tidyr::unite("colour_vars",
                   c(dplyr::all_of(.env$colourVars)), remove = FALSE, sep = "; ")
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
