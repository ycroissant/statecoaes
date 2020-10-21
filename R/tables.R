#' Table de contingence
#'
#' Une table de contingence rassemble les fréquences de toutes les
#' combinaisons de deux variables catégorielles sous la forme d'un
#' tableau à double entrée, avec les différentes modalités de la
#' première (seconde) variable en ligne (colonne). Les fonctions
#' `joint`, `marginal` et `conditional` permettent de calculer ces
#' trois distributions à partir d'une table de contingence (en
#' indiquant la variable pour laquelle la distribution doit être
#' calculée pour les deux dernières). Une méthode `mean` est définie
#' pour calculer la moyenne.
#'
#' @name cont_table
#' @aliases cont_table
#' @param data un tibble
#' @param x un tibble contenant la table de contingence
#' @param y la variable sur laquelle on veut réaliser l'opération
#' @param y1 une première variable catégorielle
#' @param y2 une seconde variable catégorielle
#' @param pond une éventuelle variable contenant les pondérations à
#'     utiliser pour passer de l'échantillon à la population
#' @param total si `TRUE` (valeur par défaut), un total est ajouté au
#'     tableau
#' @param first1 the center of the first class for the first variable
#' @param last1 the center of the last class for the first variable
#' @param inflate1 the width of the last class for the first variable
#' @param first2 the center of the first class for the second variable
#' @param last2 the center of the last class for the second variable
#' @param inflate2 the width of the last class for the second variable
#' @param ... d'autres arguments
#' @param n the number of lines to print
#' @param width the width of the table to print
#' @param n_extra extra n lines
#' @return un tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows mutate filter ungroup select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names
#' @author Yves Croissant
#' @examples
#'
#' cont_table(Emploi, diplome, sexe)
#' cont_table(Emploi, diplome, sexe, pond = pondérations)
#' cont_table(Emploi, diplome, sexe) %>% conditional(sexe)
#' cont_table(Salaires, salaire, taille)
#' cont_table(Salaires, salaire, taille) %>% joint
#' cont_table(Salaires, salaire, taille) %>% joint %>% mean
#' cont_table(Salaires, salaire, taille) %>% marginal(taille)
#' cont_table(Salaires, salaire, taille) %>% conditional(taille) %>% mean
#' 
cont_table <- function(data, y1, y2, pond = NULL,
                       total = TRUE,
                       first1 = NULL, last1 = NULL, inflate1 = NULL,
                       first2 = NULL, last2 = NULL, inflate2 = NULL){
    pond_lgc <- deparse(substitute(pond)) != "NULL"
    y1_name <- deparse(substitute(y1))
    y2_name <- deparse(substitute(y2))
    if (! pond_lgc) ct <- data %>% group_by({{ y1 }}, {{ y2 }}) %>%
                        summarise(eff = n()) %>% ungroup
    else  ct <- data %>% group_by({{ y1 }}, {{ y2 }}) %>%
              summarise(eff = sum({{ pond }})) %>% ungroup
#    if (na.rm) ct <- na.omit(ct)
    ct <- ct %>% mutate_if(is.factor, as.character)
    if (total){
        mg_1 <- ct %>% group_by({{ y1 }}) %>%
            summarise(eff = sum(eff)) %>%
            bind_cols("{{ y2 }}" := "Total")
        mg_2 <- ct %>% group_by({{ y2 }}) %>%
            summarise(eff = sum(eff)) %>%
            bind_cols("{{ y1 }}" := "Total")
        mg_tot <- summarise(mg_1, eff = sum(eff)) %>%
            bind_cols("{{ y2 }}" := "Total",
                      "{{ y1 }}" := "Total")
        ct <- bind_rows(ct, mg_1, mg_2, mg_tot)
    }
    limits = list(list(first = first1, last = last1, inflate = inflate1),
                  list(first = first2, last = last2, inflate = inflate2))
    names(limits) <- c(y1_name, y2_name)
    structure(ct,
              class = c("cont_table", class(ct)),
              total = total,
              limits = limits)
}

    
#' @rdname cont_table
#' @export
mean.cont_table <- function(x, ...){
    if (! is.null(attr(x, "y"))){
        y_name <- attr(x, "y")
        y <- x %>% .[[y_name]] %>% unique %>% setdiff("Total")
        limits <- attr(x, "limits")[[y_name]]
        y_ctr <- cls2val(y, 0.5,
                         xfirst = limits$first,
                         xlast = limits$last,
                         inflate = limits$inflate)
        names(y_ctr) <- y
    }
    else y_name <- NULL
    if (length(x) == 2){
        x[[y_name]] <- y_ctr[x[[y_name]]]
        x <- x %>% summarise(mean = sum(!! as.symbol(y_name) * f)) %>%
            set_names(y_name)
    }
    else{
        if (! is.null(y_name)){
            # conditional distribution
            cond_name <- setdiff(names(x)[1:2], y_name)
            x[[y_name]] <- y_ctr[x[[y_name]]]
            # put the levels of the conditional variable in the right order
            y <- x %>% pull(cond_name) %>% unique %>% tibble %>% set_names(cond_name)
            x <- x %>% group_by( !! as.symbol(cond_name)) %>%
                summarise(mean = sum( !! as.symbol(y_name) * f)) %>%
                set_names(c(cond_name, y_name))
            x <- y %>% left_join(x)
        }
        else{
            limits <- attr(x, "limits")
            y1 <- x %>% pull(1) %>% unique %>% setdiff("Total")
            y1_ctr <- cls2val(y1, 0.5,
                              xfirst = limits[[1]]$first,
                              xlast = limits[[1]]$last,
                              inflate = limits[[1]]$inflate)
            names(y1_ctr) <- y1
            x[[1]] <- y1_ctr[x[[1]]]
            
            y2 <- x %>% pull(2) %>% unique %>% setdiff("Total")
            y2_ctr <- cls2val(y2, 0.5,
                              xfirst = limits[[2]]$first,
                              xlast = limits[[2]]$last,
                              inflate = limits[[2]]$inflate)
            names(y2_ctr) <- y2
            x[[2]] <- y2_ctr[x[[2]]]
            x <- x %>% summarise(mean1 = sum( !! as.symbol(names(x)[1]) * f / sum(f)),
                                 mean2 = sum( !! as.symbol(names(x)[2]) * f / sum(f))) %>%
                set_names(c(names(x)[1], names(x)[2]))
        }
    }
    x
}


fun.cont_table <- function(x, fun = weighted.mean, ...){
    if (! is.null(attr(x, "y"))){
        y_name <- attr(x, "y")
        y <- x %>% .[[y_name]] %>% unique %>% setdiff("Total")
        limits <- attr(x, "limits")[[y_name]]
        y_ctr <- cls2val(y, 0.5,
                         xfirst = limits$first,
                         xlast = limits$last,
                         inflate = limits$inflate)
        names(y_ctr) <- y
    }
    else y_name <- NULL
    if (length(x) == 2){
        x[[y_name]] <- y_ctr[x[[y_name]]]
        x <- x %>% summarise(stat = fun(!! as.symbol(y_name), w = f, ...)) %>%
            set_names(y_name)
    }
    else{
        if (! is.null(y_name)){
            # conditional distribution
            cond_name <- setdiff(names(x)[1:2], y_name)
            x[[y_name]] <- y_ctr[x[[y_name]]]
            # put the levels of the conditional variable in the right order
            y <- x %>% pull(cond_name) %>% unique %>% tibble %>% set_names(cond_name)
            x <- x %>% group_by( !! as.symbol(cond_name)) %>%
                summarise(stat = fun(!! as.symbol(y_name), w = f, ...)) %>%
                set_names(c(cond_name, y_name))
            x <- y %>% left_join(x)
        }
        else{
            limits <- attr(x, "limits")
            y1 <- x %>% pull(1) %>% unique %>% setdiff("Total")
            y1_ctr <- cls2val(y1, 0.5,
                              xfirst = limits[[1]]$first,
                              xlast = limits[[1]]$last,
                              inflate = limits[[1]]$inflate)
            names(y1_ctr) <- y1
            x[[1]] <- y1_ctr[x[[1]]]
            
            y2 <- x %>% pull(2) %>% unique %>% setdiff("Total")
            y2_ctr <- cls2val(y2, 0.5,
                              xfirst = limits[[2]]$first,
                              xlast = limits[[2]]$last,
                              inflate = limits[[2]]$inflate)
            names(y2_ctr) <- y2
            x[[2]] <- y2_ctr[x[[2]]]
            x <- x %>% summarise(stat1 = fun(!! as.symbol(names(x)[1]), w = f, ...),
                                 stat2 = fun(!! as.symbol(names(x)[2]), w = f, ...)) %>%
                set_names(c(names(x)[1], names(x)[2]))
        }
    }
    x
}

total.omit <- function(x) x[ x[[1]] != "Total" & x[[2]] != "Total", ]

#' @rdname cont_table
#' @export
joint <- function(x){
    x %>% total.omit %>% mutate(eff = eff / sum(eff)) %>% rename(f = eff)
}

#' @rdname cont_table
#' @export
marginal <- function(x, y = NULL){
    limits <- attr(x, "limits")
    N <- x %>% total.omit %>% summarise(N = sum(eff)) %>% pull(N)
    y_name <- deparse(substitute(y))
    y_name <- ifelse(y_name == "NULL", NA, y_name)
    if (is.na(y_name)) stop("y should be indicated")
    if (! y_name %in% names(x)[1:2]) stop(y_name, "unknown")
    # put the classes in the right order
    y_ord <- tibble(unique(setdiff(x[[y_name]], "Total"))) %>% set_names(y_name)
    x <- x %>% total.omit %>% group_by( {{ y }}) %>%
        summarise(f = sum(eff)) %>% mutate(f = f / sum(f))
    x <- y_ord %>% left_join(x)
    structure(x, class = c("cont_table", class(x)), y = y_name, limits = limits)
}

#' @rdname cont_table
#' @export
conditional <- function(x, y = NULL){
    limits <- attr(x, "limits")
    y_name <- deparse(substitute(y))
    if (y_name == "NULL") y_name <- NA
    if (is.na(y_name)) stop("the variable should be indicated")
    if (! y_name %in% names(x)[1:2]) stop(paste("variable", y_name, "unknown"))
    cond_name <- setdiff(names(x)[1:2], y_name)
    x <- x %>% total.omit %>% group_by(!! as.symbol(cond_name)) %>%
        mutate(eff = eff / sum(eff)) %>% ungroup %>% rename(f = eff)
    structure(x, class = c("cont_table", class(x)), y = y_name, limits = limits)
}
    

#' @rdname cont_table
#' @export
pre_print.cont_table <- function(x){
    if (length(x) == 3){
        x <- x %>% pivot_wider(names_from = 2, values_from = 3)
        if ("NA" %in% names(x)) x <- x %>% rename(Total = `NA`)
        if (any(is.na(x[[1]])))
            x[[1]] <- ifelse(is.na(x[[1]]), "Total", x[[1]])
    }
    x
}

#' @rdname cont_table
#' @export
format.cont_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
    x <- pre_print(x)
    class(x) <- setdiff(class(x), "cont_table")
    format(x, ..., n = n, width = width, n_extra = n_extra)
}    



