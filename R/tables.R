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
#' @param abs une valeur logique : `TRUE` pour renvoyer des
#'     fréquences absolues, `FALSE` (la valeur par défaut) pour des
#'     fréquences absolues,
#' @param pct une valeur logique : `TRUE` pour renvoyer des
#'     pourcentages (20.2 au lieu de 0.202)
#' @param cumul une valeur logique (`FALSE` est la valeur par
#'     défaut). Si `TRUE`, les fréquences cummulées sont renvoyées en
#'     plus des fréquences
#' @param pond une éventuelle variable contenant les pondérations à
#'     utiliser pour passer de l'échantillon à la population
#' @param na.rm la valeur par défaut est `TRUE`, les observations
#'     pour lesquelles la valeur de `x` est manquante sont retirées de
#'     l'échantillon
#' @return un tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows `%>%` n
#' @importFrom stats na.omit
#' @importFrom rlang `:=`
#' @author Yves Croissant
#' @examples
#'
#' freq_table(Emploi, activité, abs = TRUE, cumul = FALSE)
#' freq_table(Emploi, activité, abs = TRUE, cumul = TRUE)
#' freq_table(Emploi, activité, abs = TRUE, cumul = FALSE, pond = pondérations)
#' freq_table(Emploi, activité, cumul = TRUE)
#' freq_table(Emploi, activité, cumul = TRUE, pond = pondérations)
freq_table <- function(data, x, abs = FALSE, pct = FALSE, cumul = FALSE, pond = NULL, na.rm = TRUE){
    pond_lgc <- deparse(substitute(pond)) != "NULL"
    if (! pond_lgc) ct <- data %>% group_by({{ x }}) %>% summarise(eff = n())
    else  ct <- data %>% group_by({{ x }}) %>% summarise(eff = sum({{ pond }}))
    if (na.rm) ct <- na.omit(ct)
    ct <- ct %>% mutate_if(is.factor, as.character)
    if (! abs) ct <- ct %>% mutate(eff = eff / sum(eff))
    mg <- ct %>% summarise(eff = sum(eff)) %>% bind_cols("{{ x }}" := "Total")
    ct <- bind_rows(ct, mg)
    if (! abs & pct) ct <- ct %>%
                           mutate_if(is.numeric, function(x) x * 100)
    if (cumul) ct <- ct %>% mutate(cumeff = cumsum(eff),
                                   cumeff = ifelse({{ x }} == "Total",
                                                   cumeff[length(cumeff) - 1],
                                                   cumeff))
    ct
}


#' Table de contingence
#'
#' Une table de contingence rassemble les fréquences de toutes les
#' combinaisons de deux variables catégorielles sous la forme d'un
#' tableau à double entrée, avec les différentes modalités de la
#' première (seconde) variable en ligne (colonne)
#'
#' @name cont_table
#' @aliases cont_table
#' @param data un tibble
#' @param x1 une première variable catégorielle
#' @param x2 une seconde variable catégorielle
#' @param abs une valeur logique : `TRUE` pour renvoyer des fréquences
#'     absolues, `FALSE` (la valeur par défaut) pour des fréquences
#'     absolues,
#' @param pct une valeur logique : `TRUE` pour renvoyer des
#'     pourcentages (20.2 au lieu de 0.202)
#' @param marge permet de calculer des fréquences conditionnelles en
#'     lignes (`marge = 1`) ou en colonnes (`marge = 2`) ; avec la
#'     valeur par défaut de `0`, les fréquences jointes et les
#'     fréquences marginales sont calculées.
#' @param pond une éventuelle variable contenant les pondérations à
#'     utiliser pour passer de l'échantillon à la population
#' @param na.rm la valeur par défaut est `TRUE`, les observations
#'     pour lesquelles la valeur de `x` est manquante sont retirées de
#'     l'échantillon
#' @return un tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows mutate filter ungroup select
#' @importFrom tidyr pivot_wider
#' @author Yves Croissant
#' @examples
#'
#' cont_table(Emploi, diplome, sexe)
#' cont_table(Emploi, diplome, sexe, pond = pondérations)
#' cont_table(Emploi, diplome, sexe, pond = pondérations, pct = TRUE)
#' cont_table(Emploi, diplome, sexe, pond = pondérations, pct = TRUE, marge = 1)
#' cont_table(Emploi, diplome, sexe, pond = pondérations, pct = TRUE, marge = 2)
#' 
cont_table <- function(data, x1, x2, abs = FALSE, pct = FALSE, marge = 0, pond = NULL, na.rm = TRUE){
    pond_lgc <- deparse(substitute(pond)) != "NULL"
    if (! pond_lgc) ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>% summarise(eff = n()) %>% ungroup
    else  ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>% summarise(eff = sum({{ pond }})) %>% ungroup
    if (na.rm) ct <- na.omit(ct)
    ct <- ct %>% mutate_if(is.factor, as.character)
    if (marge == 0 & ! abs) ct <- ct %>% mutate(eff = eff / sum(eff))
    if (marge == 1) ct <- ct %>% group_by({{ x1 }}) %>% mutate(eff = eff / sum(eff))
    if (marge == 2) ct <- ct %>% group_by({{ x2 }}) %>% mutate(eff = eff / sum(eff))
    mg_1 <- ct %>% group_by({{ x1 }}) %>%
        summarise(eff = sum(eff)) %>%
        bind_cols("{{ x2 }}" := "Total")
    mg_2 <- ct %>% group_by({{ x2 }}) %>%
        summarise(eff = sum(eff)) %>%
        bind_cols("{{ x1 }}" := "Total")
    mg_tot <- summarise(mg_1, eff = sum(eff)) %>%
        bind_cols("{{ x2 }}" := "Total",
                  "{{ x1 }}" := "Total")
    ct <- bind_rows(ct, mg_1, mg_2, mg_tot)
    ct <- ct %>% pivot_wider(names_from = {{ x2 }}, values_from = eff)
    if (marge == 2) ct <- select(ct, - Total)
    if (marge == 1) ct <- filter(ct, {{ x1 }} != "Total" | is.na({{ x1 }}))
    if (! abs & pct) ct <- ct %>% mutate_if(is.numeric, function(x) x * 100)
    ct
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
#' @param na.rm la valeur par défaut est `TRUE`, les observations
#'     pour lesquelles la valeur de `x` est manquante sont retirées de
#'     l'échantillon
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
cond_table <- function(data, x, x1, x2 = NULL, fun = mean, na.rm = TRUE){
    x2_lgc <- deparse(substitute(x2)) != "NULL"
    mgtot <- data %>% summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
        bind_cols("{{ x1 }}" := "Total")
    if (x2_lgc){
        mgtot <- mgtot %>% bind_cols("{{ x2 }}" := "Total")
        ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>%
            summarise(stat = fun({{ x }}, na.rm = na.rm))
        mg2 <- data %>% group_by({{ x1 }}) %>%
            summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
            bind_cols("{{ x2 }}" := "Total")
        mg3 <- data %>% group_by({{ x2 }}) %>%
            summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
            bind_cols("{{ x1 }}" := "Total")
        ct <- bind_rows(ct, mg2, mg3, mgtot)
        ct <- ct %>% pivot_wider(names_from = {{ x2 }}, values_from = stat)
    }
    else{
        ct <- data %>% group_by({{ x1 }}) %>%
            summarise(stat = fun({{ x }}, na.rm = na.rm))
        mg <- data %>% summarise(stat = fun({{ x }}, na.rm = na.rm)) %>%
            bind_cols("{{ x1 }}" := "Total")
        ct <- bind_rows(ct, mg)
    }
    ct
}

