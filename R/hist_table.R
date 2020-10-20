
#' Histogrammes
#'
#' Calcule les effectifs par classe pour une variable numérique
#' 
#' @name hist_table
#' @aliases hist_table
#' @param data un tibble
#' @param x la variable considérée (nécessairement numérique)
#' @param cols une chaîne de caractère contenant les lettres `n` pour
#'     nombre, `f` pour fréquence et `p` pour pourcentage ; les séries
#'     cumulées sont obtenues en indiquant les mêmes lettres en
#'     majuscule. Par rapport à `freq_table`, la lettre `d` peut
#'     également être indiquée pour calculer des densités
#' @param vals les valeurs de la variable renvoyées ; `x` pour le
#'     centre de la classe, `l` et `u` pour les limites inférieure et
#'     supérieure, `a` pour l'amplitude
#' @param breaks un vecteur de limites de classes
#' @param xfirst une valeur numérique indiquant le centre de la
#'     première classe
#' @param xlast une valeur numérique indiquant le centre de la dernière
#'     classe
#' @param right un booléen indiquant si les classes doivent être
#'     fermées (`right = TRUE`) ou fermée (`right = FALSE`) à droite
#' @param total un total doit il être renvoyé ?
#' @param inflate dans le cas où la valeur centrale de la dernière
#'     classe n'est pas renseignée, elle est fixée à la borne
#'     inférieure plus ce coefficient multiplié par la moitié de
#'     l'amplitude de la classe précédente
#' @return un tibble contenant les valeurs de `vals` et de `cols`
#'     spécifiées
#' @export
#' @importFrom dplyr all_of slice arrange tibble
#' @author Yves Croissant
#' @examples
#'
#' # price is a numeric variable, a vector of breaks should be provided
#' Padoue %>% hist_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400), right = TRUE)
#' Padoue %>% hist_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400), right = TRUE, cols = "fd", vals = "xa")
#' # salaire is a factor that represents the classes
#' Salaires %>% hist_table(salaire, "d")
#' # a breaks argument is provided to reduce the number of classes
#' Salaires %>% hist_table(salaire, breaks = c(10, 20, 30, 40, 50))
#' 
hist_table <- function(data, x, cols = "n", vals = "x", breaks = NULL,
                       xfirst = NULL, xlast = NULL, right = NULL,
                       total = FALSE, inflate = NULL){
    if (is.null(xlast) & is.null(inflate)) inflate <- 1
    # check wether the computation of densities is required and if so
    # create a boolean and remove d from cols
    cols_vec <- strsplit(cols, "")[[1]]
    remove_counts <- FALSE
    if (any(c("d", "m", "M") %in% cols_vec)){
        compute_densities <- ifelse("d" %in% cols_vec, TRUE, FALSE)
        compute_masses <- ifelse("m" %in% cols_vec, TRUE, FALSE)
        compute_cummasses <- ifelse("M" %in% cols_vec, TRUE, FALSE)
        cols_vec <- setdiff(cols_vec, c("m","d", "M"))
        if (! "n" %in% cols_vec){
            cols_vec <- c("n", cols_vec)
            remove_counts <- TRUE
        }
        cols <- paste(cols_vec, collapse = "")
        ## if (compute_densities) cols_vec <- c(cols_vec, "d")
        ## if (remove_counts) cols_vec <- setdiff(cols_vec, "n")
    }
    else{
        compute_densities <- FALSE
        compute_masses <- FALSE
        compute_cummasses <- FALSE
    }
    vals_vec <- strsplit(vals, "")[[1]]
    vals_na <- setdiff(vals_vec, c("a", "x", "l", "u"))
    if (length(vals_na) > 0)
        stop(paste(paste(sort(vals_na), collapse = ", "),
                   paste(" are provided in the vals argument but are not regular values", sep = ""),
                   sep = ""))
    is_numeric_x <- is.numeric(data %>% pull({{ x }}))
    if (is_numeric_x){
        # x is numeric, cut it according to the break and the left argument and then count
        if (is.null(breaks)) stop("the argument breaks should be provided")
        # right = TRUE is the default value of cut, so keep it at is
        if (is.null(right)) right <- TRUE
        # if the max value of break is lower than the maximum value of
        # x, add Inf to the vectors of breaks
        if (max(breaks) < max(data %>% pull({{ x }}))) breaks <- c(breaks, Inf)
        # if the min value of break is greater than the minimum value
        # of x, add either 0 (if min(x) >= 0) or -Inf to the vector of breaks
        if (min(breaks) > min(data %>% pull({{ x }})))
            breaks <- c(ifelse(min(data %>% pull({{ x }})) < 0, - Inf, 0), breaks)
        data <- data %>% mutate("{{ x }}" := cut({{ x }}, breaks, right = right))
    }
    else{
        if (! is.null(breaks)) data <- data %>% mutate("{{ x }}" := recut({{ x }}, breaks = breaks))
    }
    res <- freq_table(data, {{ x }}, cols = cols, total = FALSE)

    if ((any(c("x", "a") %in% vals_vec)) | compute_densities){
        res <- res %>% mutate(x = cls2val({{ x }}, 0.5, xfirst = xfirst,
                                          xlast = xlast, inflate = inflate))
    }
    if ((any(c("l", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(l = cls2val({{ x }}, 0))
    if ((any(c("u", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(u = cls2val({{ x }}, 1, inflate = inflate, xlast = xlast))
    if (("a" %in% vals_vec) | compute_densities){
        NR <- nrow(res)
        xlast_inf <- res %>% slice(NR) %>% pull(u) %>% is.infinite
        if (xlast_inf){
            res <- res %>% slice(NR) %>% mutate(u2 = l + 2 * (x - l)) %>% pull(u2)
            res <- res %>% mutate(u2 = ifelse(is.infinite(u), u2, u))
            res <- res %>% mutate(a = u2 - l) %>% select(- u2)
        }
        else res <- res %>% mutate(a = u - l)
        if (! "l" %in% vals_vec) res <- res %>% select(- l)
        if (! "u" %in% vals_vec) res <- res %>% select(- u)
        if (compute_densities) res <- res %>% mutate(d = n / sum(n) / a)
        if (! "a" %in% vals_vec) res <- res %>% select(- a)
    }
    if (compute_masses | compute_cummasses){
        res <- res %>% mutate(m = n * x,
                              m = m / sum(m))
        if (compute_cummasses) res <- res %>% mutate(M = cumsum(m))
        if (! compute_masses) res <- res %>% select(- m)
    }
    if (remove_counts) res <- res %>% select(- n)
    cols_pos <- match(c("n", "f", "p", "N", "F", "P", "d", "m", "M"),
                      names(res)) %>% na.omit %>% sort
    vals_pos <- match(c("x", "l", "a", "u"), names(res)) %>% na.omit %>% sort
    res <- res %>% select({{ x }}, all_of(c(vals_pos, cols_pos)))
    structure(res, class = c("hist_table", class(res)))
}



#' Put a tibble in form to plot
#'
#' Convert a tibble built using hist_table in a shape that make it
#' easy to plot
#'
#' #'
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
    
#' Methods for hist_table objects
#'
#' Functions and methods to compute the median, the mean, the mode,
#' the medial and quantiles for hist_table objects
#' 
#' @name hist_table.methods
#' @aliases hist_table.methods
#' @param x a hist_table object,
#' @param ... further arguments
#' @param na.rm a boolean, if `TRUE` missing values are removed
#' @param tbl if `TRUE` a tibble containing the class and the
#'     densities is returned
#' @param probs the probabilities for which the quantiles have to be
#'     computed
#' @return a tibble if `tbl` is `TRUE`, a numeric otherwise
#' @export
#' @importFrom stats quantile median
#' @importFrom purrr map_dbl map_dfr
#' @author Yves Croissant
#' @examples
#'
#' z <- Salaires %>% hist_table(salaire)
#' z %>% median
#' z %>% medial
#' z %>% modval
#' z %>% modval(tbl = TRUE)
#' z %>% quantile(probs = c(0.25, 0.5, 0.75))
#' z %>% quantile(probs = c(0.25, 0.5, 0.75), tbl = TRUE)
#' z %>% tantile(probs = c(0.25, 0.5, 0.75), tbl = TRUE)
mean.hist_table <- function(x, ..., na.rm = TRUE, tbl = FALSE){
    x <- x %>% rename(cls = 1)
    if (! "f" %in% names(x)) x <- x %>% mutate(f = compute_freq(.))
    xb <- x %>% summarise(xb = sum(x * f, na.rm = na.rm)) %>% pull(xb)
    if (tbl){
        if (! "d" %in% names(x)) x <- x %>% mutate(d = compute_dens(.))
        d <- x %>% mutate(xu = cls2val(cls, 1)) %>% filter(xb < xu) %>% slice(1) %>% pull(d)
        tibble(x = xb, d = d)
    }
    else xb
}
        

#' @name hist_table.methods
#' @export
modval <- function(x, ...)
    UseMethod("modval")


#' @name hist_table.methods
#' @export
modval.hist_table <- function(x, ..., tbl = FALSE){
    x <- x %>% rename(cls = 1)
    if (! "d" %in% names(x)) x <- x %>% mutate(d = compute_dens(.))
    pos <- x %>% summarise(pos = which.max(d)) %>% pull(pos)
    if (! tbl) x %>% slice(pos) %>% pull(cls) %>% as.character
    else slice(x, pos) %>% select(cls, d)
}

tile <- function(x, y = NULL, probs = NULL, tbl = FALSE){
    xlast <- x %>% pull(x) %>% rev %>% .[1]
    if (! inherits(x, "hist_table")) stop("x should be a hist_table object")
    if (is.null(y)) stop("don't know what kind of tiles to compute")
    if (! y %in% c("M", "F")) stop("y should be either F or M")
    if (is.null(probs)) stop("don't know what values of tiles to compute")
    if (! y %in% names(x)){
        if (! "f" %in% names(x)) x <- x %>% mutate(f = compute_freq(.))
        if (y == "F") x <- x %>% mutate(F = cumsum(f))
        if (y == "M"){
            if (! "m" %in% names(x)){
                x <- x %>% mutate(m = f * x,
                                  m = m / sum(m))
            }
            x <- x %>% mutate(M = cumsum(f * x))
        }
    }
    if (tbl){
        if (! "d" %in% names(x)) x <- x %>% mutate(d = compute_dens(.))
        x <- x %>% rename(y = y) %>% select(1, x, y, d)
    } 
    else x <- x %>% rename(y = y) %>% select(1, x, y)
    get_quant <- function(aprob){
        pos <- x %>% mutate(zz = y > aprob) %>% pull(zz) %>% which
        id <- pos[1]
        if (id == 1L) Fm1 <- 0
        else Fm1 <- x %>% pull(y) %>% .[id - 1]
        F <- x %>% pull(y) %>% .[id]
        x_cls <- x %>% pull(1)
        a_x_cls <- as.character(x_cls[id])
        if (tbl) a_d <- x %>% pull(d) %>% .[id]
        a_quant <- acls2val(a_x_cls, (aprob - Fm1) / (F - Fm1), xlast = xlast)
        if (tbl) list(cls = a_x_cls, p = aprob, d = a_d, q = a_quant) else a_quant
    }
    if (tbl) map_dfr(probs, get_quant) else map_dbl(probs, get_quant)
}
        

#' @name hist_table.methods
#' @export
quantile.hist_table <- function(x, probs = c(0.25, 0.5, 0.75), tbl = FALSE, ...){
    tile(x, y = "F", probs = probs, tbl = tbl)
}


#' @name hist_table.methods
#' @export
median.hist_table <- function(x, na.rm, ..., tbl = FALSE){
    quantile(x, 0.5, tbl = tbl)
}

#' @name hist_table.methods
#' @export
medial <- function(x, tbl = FALSE)
    UseMethod("medial")

#' @name hist_table.methods
#' @export
medial.hist_table <- function(x, tbl = FALSE){
    tantile(x, 0.5, tbl = tbl)
}


#' @name hist_table.methods
#' @export
tantile <- function(x, probs = c(0.25, 0.5, 0.75), tbl = FALSE, ...)
    tile(x, y = "M", probs = probs, tbl = tbl)


#' @name hist_table.methods
#' @export
gini <- function(x){
    if (! inherits(x, "hist_table")) stop("x should be a hist_table object")
    if (any(! c("F", "M") %in% names(x))){
        x <- x %>% mutate(f = compute_freq(.))
        if (! "F" %in% names(x)) x <- x %>% mutate(F = cumsum(f))
        if (! "M" %in% names(x)) x <- x %>% mutate(m = f * x,
                                                   m = m / sum(m),
                                                   M = cumsum(m))
    }
    x %>% add_row(F = 0, M = 0, .before = 0) %>%
        mutate(tz = (F - lag(F)) * (lag(M) + M) / 2) %>%
        summarise(g = 2 * (0.5 - sum(tz, na.rm = TRUE))) %>%
        pull(g)    
}


compute_bonds <- function(x, xlast = NULL, xfirst = NULL, inflate = NULL){
    xu <- cls2val(x, 1L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    xl <- cls2val(x, 0L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    tibble(x, xl, xu)
}

compute_widths <- function(x, xlast = NULL, xfirst = NULL, inflate = NULL){
    xu <- cls2val(x, 1L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    xl <- cls2val(x, 0L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    xu - xl
}

compute_freq <- function(x){
    if (! inherits(x, "hist_table")) stop("x should be an hist_table object")
    if (! "f" %in% names(x)){
        if (any(c("f", "p", "n") %in% names(x))){
            col <- na.omit(match(c("f", "p", "n"), names(x)))[1]
            f <- x[[col]]
            f <- f / sum(f)
        }
        else{
            if (any(c("F", "P", "N") %in% names(x))){
                col <- na.omit(match(c("F", "P", "N"), names(x)))[1]
                f <- x[[col]]
                f <- c(f[1], f[-1] - f[- length(f)])
                f <- f / sum(f)
            }
            else stop("the table should contain any of f, p, n, F, P, N")
        }
        f
    }
    else pull(x, f)
}
    
compute_dens <- function(x, xlast = NULL, xfirst = NULL, inflate = NULL){
    if (! inherits(x, "hist_table")) stop("x should be an hist_table object")
    if (! "d" %in% names(x)){
        f <- compute_freq(x)
        a <- compute_widths(x[[1]], xlast = xlast, xfirst = xfirst, inflate = NULL)
        d <- f /a
        d
    }
    else pull(x, d)
}
