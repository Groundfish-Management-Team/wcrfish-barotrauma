#' Calculate cumulative mortality rates
#'
#' @param ci The estimated mortality that is by default specified
#' to be the 90% upper confidence interval
#' @param lt Additional long-term moratitly
#' @param add Additional unaccounted for mortality
#'
#'
calc_mort <- function(ci, lt, add){
	cumulative_mortality <- 1 - (1 - ci) * (1 - lt) * (1 - add)
	return(cumulative_mortality)
}


