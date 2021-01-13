#' Moyennes mobiles
#'
#' Calcul des moyennes mobiles pour des données mensuelles ou
#' trimestrielles
#' 
#' @name moving_average
#' @aliases moving_average
#' @param data un tibble
#' @param freq soit `month` pour des données mensuelles ou `quarter`
#'     pour des données trimestrielles
#' @return un tibble
#' @export
#' @importFrom zoo rollmean
#' @author Yves Croissant
moving_average <- function(data, freq = c("month", "quarter")){
    freq <- match.arg(freq)
    freq <- ifelse(freq == "month", 12, 4)
    x <- zoo::rollmean(data, freq, fill = NA, align = "center")
    zoo::rollmean(x, 2, fill = NA, align = "right")
}


