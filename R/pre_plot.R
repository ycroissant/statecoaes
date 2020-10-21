#' Put a tibble in form to plot
#'
#' Convert a tibble built using hist_table in a shape that make it
#' easy to plot
#'
#' 
#' @name pre_plot
#' @aliases pre_plot
#' @param x a tibble returned by the `hist_table` function, it
#'     should contains the center of the classes (`x`) and at least
#'     one measure of the frequencies or densities (one of `f`, `n`,
#'     `p`, `d`)
#' @param y mandatory argument if the tibble contains more than one
#'     frequency or density
#' @param plot one of `histogram` (the default) and `freqpoly` ; in
#'     the first case a tibble is returned with columns `x`, `y`,
#'     `xend`, `yend` and in the second case `x` and `y`.
#' @return a tibble
#' @importFrom dplyr desc
#' @export
#' @author Yves Croissant
#' @examples
#' library("ggplot2")
#' pad <- Padoue %>% hist_table(price, breaks = c(100, 200, 300, 400, 500, 1000), right = TRUE, cols = "Npd")
#' pad %>% pre_plot(y = "d") %>% ggplot() + geom_polygon(aes(x, y))
#' pad %>% pre_plot(y = "d", plot = "freqpoly") %>% ggplot() + geom_line(aes(x, y))
#' ## A pie chart
#' Salaires %>% freq_table(secteur, "p", total = FALSE) %>%
#'   pre_plot("p") %>% ggplot(aes(x = 2, y = p, fill = secteur)) +
#'   geom_col() + geom_text(aes(y = ypos, label = round(p))) +
#'   coord_polar(theta = "y")
#' 
pre_plot <- function(x, y = NULL, plot = NULL, ...)
    UseMethod("pre_plot")

#' @rdname pre_plot
#' @export
pre_plot.hist_table <- function(x, y = NULL, plot = "histogram"){
    data <- x
    if (! "x" %in% names(data))
        stop("the table should contains the center of the classes")
    if (is.null(y)){
        ys <- c("d", "f", "p", "n")
        cols <- match(names(data), ys) %>% na.omit %>% as.numeric
        if (length(cols) == 0L)
            stop("nothing to plot, the tibble should contain either d, f or n")
        if (length(cols) > 1L)
            stop("the variable to plot should be specified")
        data <- rename(data, y = ys[cols]) %>%
            select(1, x, y)
    }
    else{
        data <- data %>% select(1, x, y = matches(paste("^[", y, "]{1}$", sep = ""), ignore.case = FALSE))
    }        
    K <- nrow(data)
    xu <- data %>% pull(1) %>% cls2val(1)
    xl <- data %>% pull(1) %>% cls2val(0)
    x <- data %>% pull(x)
    xu[K] <- xl[K] + 2 * (x[K] - xl[K])
    xl[1] <- xu[1] - 2 * (xu[1] - x[1])
    if (plot == "histogram"){
        data <- data %>%
            rename(cls = 1) %>%
            select(cls, y_ne = y) %>%
            mutate(x_sw = xl,
                   y_sw = 0,
                   x_nw = xl,
                   y_nw = y_ne,
                   x_ne = xu,
                   y_ne = y_ne,
                   x_se = xu,
                   y_se = 0) %>%
            pivot_longer( - cls) %>%
            separate(name, into = c("axe", "pos")) %>%
            pivot_wider(names_from = axe, values_from = value) %>%
            mutate(pos = factor(pos, levels = c("sw", "nw", "ne", "se"))) %>%
            arrange(desc(cls), pos)
    }
    if (plot == "freqpoly"){
        xo <- xl[1] - (x[1] - xl[1])
        xs <- xu[K] + (xu[K] - x[K])
        data <- data %>%
            select(- 1) %>%
            add_row(x = xo, y = 0, .before = 0) %>%
            add_row(x = xs, y = 0, .after = Inf)
    }
    structure(data, class = c("hist_table", class(data)))
}

#' @rdname pre_plot
#' @export
pre_plot.freq_table <- function(x, y = NULL, plot = NULL, ...){
    if (is.null(y)) y <- names(x)[2]
    x <- x %>% select(1, all_of(y))
    z <- x %>% names %>% .[1]
    x %>% total.omit %>% arrange(desc(!! as.symbol(z))) %>%
        mutate(ypos = cumsum(!! as.symbol(y)) - 0.5 * !! as.symbol(y))
}
