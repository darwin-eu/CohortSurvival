#' Survival summary
#'
#' @param x Result from estimateSingleEventSurvival
#'
#' @return
#' @export
#'
#' @examples
survivalSummary <- function(x){
attr(x, "summary")
}
