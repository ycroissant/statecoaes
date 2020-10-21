#' Generic functions provided by the statecoaes package
#'
#' Some generics were created, some of them don't exist in base R (to
#' compute the mode and the mediale of a distribution for example) or
#' because the functions exist but are not generic (this is the case
#' of var, sd, mad, which are replaced by our variance, stdev and
#' madev generic functions
#'
#' @name generics
#' @param x the main argument, basically a tibble,
#' @param na.rm should the missing values be stripped before the
#'     computation of the statistic,
#' @param tbl, if `FALSE` a numeric is returned, if `TRUE` a tibble
#'     with the density is returned, which can be usefull for central
#'     statistics like madev, modval and medial
#' @param ... further arguments
#' @return a numeric or a tibble
#' @author Yves Croissant
#' 
variance <- function(x, ..., na.rm = TRUE)
    UseMethod("variance")


#' @rdname generics
#' @export
stdev <- function(x, ..., na.rm = TRUE)
    UseMethod("stdev")

#' @rdname generics
madev <- function(x, ..., na.rm = TRUE)
    UseMethod("madev")

#' @rdname generics
#' @export
modval <- function(x, ...)
    UseMethod("modval")

#' @rdname generics
#' @export
pre_print <- function(x)
    UseMethod("pre_print")

#' @rdname generics
#' @export
medial <- function(x, tbl = FALSE)
    UseMethod("medial")
