#' Tables de Fréquence
#'
#' Une table de fréquence est adaptée pour les variables
#' catégorielles, elle renvoit les différentes modalités et les
#' fréquences associées
#'
#' @name freq_table
#' @aliases freq_table
#' @param data un tibble
#' @param x une variable catégorielle
#' @param cols une chaîne de caractère contenant les lettres `n` pour
#'     nombre, `f` pour fréquence et `p` pour pourcentage ; les séries
#'     cumulées sont obtenues en indiquant les mêmes lettres en
#'     majuscule
#' @param weights une éventuelle variable contenant les pondérations à
#'     utiliser pour passer de l'échantillon à la population
#' @param na.rm la valeur par défaut est `TRUE`, les observations
#'     pour lesquelles la valeur de `x` est manquante sont retirées de
#'     l'échantillon
#' @param total si `TRUE` (valeur par défaut), un total est ajouté au
#'     tableau
#' @param max dans le cas où la variable est numérique entière, cet
#'     argument indique que les valeurs supérieures ou égales à `max`
#'     seront regroupées
#' @param n le nombre de ligne à imprimer (pour `format`)
#' @param width la largeur du tableau à imprimer (pour `format`)
#' @param n_extra le nombre de colonnes suplémentaires décrites (pour `format`)
#' @param ... d'autres arguments (pour `format`)
#' @return un objet de class `freq_table` qui hérite de `tbl_df`
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows `%>%` n matches pull summarise_all
#' @importFrom stats na.omit
#' @importFrom rlang `:=`
#' @author Yves Croissant
#' @examples
#'
#' freq_table(Emploi, activite, "n")
#' freq_table(Emploi, activite, "nN")
#' freq_table(Emploi, activite, "fF", weights = ponderations)
#' freq_table(RGP78, enfants, "npNP")
#' freq_table(RGP78, enfants, "npNP", max = 5)
freq_table <- function(data, x, cols = "n", weights = NULL, na.rm = TRUE, total = TRUE, max = NA){
    # check whether there are some weights, if so sum the weights,
    # else count the observations
    wgts_lgc <- deparse(substitute(weights)) != "NULL"
    # check whether the variable is numeric
    x_is_num <- is.numeric(data %>% pull({{ x }}))
    if (! wgts_lgc) ct <- data %>% group_by({{ x }}) %>%
                        summarise(n = n())
    else  ct <- data %>% group_by({{ x }}) %>%
              summarise(n = sum({{ weights }}))
    # get the cols that should be returned
    cols <- strsplit(cols, "")[[1]]
    any_scaps <- any(c("f", "n", "p") %in% cols)
    if (! any_scaps) total <- FALSE
    # if max is filled, return a >= max category
    if (! is.na(max)){
        if (! x_is_num)
            stop("l'argument max n'a de sens que si la variable est numérique")
        ct1 <- filter(ct, {{ x }} < max)
        ct2 <- filter(ct, {{ x }} >= max) %>%
            summarise(n = sum(n), "{{ x }}" := max )
        ct <- ct1 %>% bind_rows(ct2)
    }
    # remove na values if required
    if (na.rm) ct <- na.omit(ct)
    # compute the frequencies if required
    if (any(c("f", "F") %in% cols)) ct <- ct %>% mutate(ct, f = n / sum(n))
    # compute the percentages if required
    if (any(c("p", "P") %in% cols)) ct <- ct %>% mutate(ct, p = n / sum(n) * 100)
    # compute the cummulative distribution if required
    if (any(c("N", "F", "P") %in% cols)){
        if ("N" %in% cols) ct <- ct %>% mutate(ct, N = cumsum(n))
        if ("F" %in% cols) ct <- ct %>% mutate(ct, F = cumsum(f))
        if ("P" %in% cols) ct <- ct %>% mutate(ct, P = cumsum(p))
    }
    if (total){
        lowcaps <- select(ct, matches("^[nfp]{1}$", ignore.case = FALSE))
        total_low <- lowcaps %>% summarise_all(sum) %>% mutate("{{ x }}" := ifelse(x_is_num, Inf, "Total"))
        ct <- ct %>% bind_rows(total_low)
    }
    ct <- select(ct, {{ x }}, !! cols)
    structure(ct, class = c("freq_table", class(ct)), max = max, total = total)
}

#' @rdname freq_table
#' @export
format.freq_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
    x <- pre_print(x)
    class(x) <- setdiff(class(x), "freq_table")
    format(x, ..., n = n, width = width, n_extra = n_extra)
}    

#' @rdname freq_table
#' @export
pre_print.freq_table <- function(x){
    max <- attr(x, "max")
    total <- attr(x, "total")
    nr <- nrow(x)
    if (total | ! is.na(max)) x[[1]] <- as.character(x[[1]])
    if (total) x[[1]][nr] <- "Total"
    if (! is.na(max)) x[[1]][nr - total] <- paste(">= ", max, sep ="")
    x
}


#' Statistique conditionnelle
#'
#' Calcule une statistique (par défaut la moyenne arithmétique) d'une
#' variable numérique pour chaque modalités d'une ou de deux variables
#' catégorielles
#'
#' 
#' @name cond_table
#' @aliases cond_table
#' @param data un tibble
#' @param x une variable numérique
#' @param x1 une première variable catégorielle
#' @param x2 une éventuelle seconde variable catégorielle
#' @param fun la fonction à appliquer (par défaut la moyenne)
#' @param na.rm la valeur par défaut est `TRUE`, les observations pour
#'     lesquelles la valeur de `x` est manquante sont retirées de
#'     l'échantillon
#' @param total si `TRUE` (valeur par défaut), un total est ajouté au
#'     tableau
#' @return un tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows
#' @author Yves Croissant
#' @examples
#'
#' cond_table(Salaires, heures, secteur)
#' cond_table(Salaires, heures, secteur, sexe)
#' cond_table(Salaires, heures, secteur, fun = var)
#' 
cond_table <- function(data, x, x1, x2 = NULL, fun = mean, na.rm = TRUE, total = TRUE){
    x2_lgc <- deparse(substitute(x2)) != "NULL"
    if (total){
        mgtot <- data %>% summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
            bind_cols("{{ x1 }}" := "Total")
    }
    if (x2_lgc){
        if (total) mgtot <- mgtot %>% bind_cols("{{ x2 }}" := "Total")
        ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>%
            summarise(stat = fun({{ x }}, na.rm = na.rm))
        if (total){
            mg2 <- data %>% group_by({{ x1 }}) %>%
                summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
                bind_cols("{{ x2 }}" := "Total")
            mg3 <- data %>% group_by({{ x2 }}) %>%
                summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
                bind_cols("{{ x1 }}" := "Total")
            ct <- bind_rows(ct, mg2, mg3, mgtot)
        }
        ct <- ct %>% pivot_wider(names_from = {{ x2 }}, values_from = stat)
    }
    else{
        ct <- data %>% group_by({{ x1 }}) %>%
            summarise(stat = fun({{ x }}, na.rm = na.rm))
        if (total){
            mg <- data %>% summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
                bind_cols("{{ x1 }}" := "Total")
            ct <- bind_rows(ct, mg)
        }
    }
    ct
}


