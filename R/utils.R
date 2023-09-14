#'  Participants contributing to a survival analysis
#'
#' @param result Result object
#'
#' @return References to the study participants contributing to
#' a given analysis
#' @export
#'
#' @examples
survivalParticipants <- function(result) {
  attr(result, "participants")
}


