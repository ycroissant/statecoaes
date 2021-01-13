#' Graphique de type radar
#'
#' Une variante de `coord_polar` qui remplace les lignes lisses en
#' segments de droite
#' 
#' @name coord_radar
#' @aliases coord_radar
#' @param theta voir `coord_polar`
#' @param start voir `coord_polar`
#' @param direction voir `coord_polar`
#' @export
#' @importFrom ggplot2 ggproto CoordPolar
#' @source [page web de Erwan Le Pennec](https://www.cmap.polytechnique.fr/~lepennec/en/post/radar/radarandparallelplots/)
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}
