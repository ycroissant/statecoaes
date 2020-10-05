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
#' @param total si `TRUE` (valeur par défaut), un total est ajouté au
#'     tableau
#' @param max dans le cas où la variable est numérique entière, cet
#'     argument indique que les valeurs supérieures ou égales à `max`
#'     seront regroupées
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
freq_table <- function(data, x, abs = FALSE, pct = FALSE, cumul = FALSE,
                       pond = NULL, na.rm = TRUE, total = TRUE, max = NA){
    pond_lgc <- deparse(substitute(pond)) != "NULL"
    if (! pond_lgc) ct <- data %>% group_by({{ x }}) %>% summarise(eff = n())
    else  ct <- data %>% group_by({{ x }}) %>%
              summarise(eff = sum({{ pond }}))
    if (! is.na(max)){
        if (! is.numeric(ct %>% pull({{ x }})))
            stop("l'argument max n'a de sens que si la variable est numérique")
        ct1 <- filter(ct, {{ x }} < max) %>%
            mutate({{ x }} := as.character({{ x }}))
        ct2 <- filter(ct, {{ x }} >= max) %>%
            summarise({{ x }} := paste(">=", max),
                      eff = sum(eff))
        ct <- ct1 %>% bind_rows(ct2)
    }
    if (na.rm) ct <- na.omit(ct)
    if (! abs) ct <- ct %>% mutate(eff = eff / sum(eff))
    if (total){
        mg <- ct %>% summarise(eff = sum(eff)) %>% bind_cols("{{ x }}" := "Total")
        ct <- ct %>% mutate({{ x }} := as.character({{ x }})) %>% bind_rows(mg)
    }
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
#' @param total si `TRUE` (valeur par défaut), un total est ajouté au
#'     tableau
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
cont_table <- function(data, x1, x2, abs = FALSE, pct = FALSE, marge = 0, pond = NULL, na.rm = TRUE, total = TRUE){
    pond_lgc <- deparse(substitute(pond)) != "NULL"
    if (! pond_lgc) ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>% summarise(eff = n()) %>% ungroup
    else  ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>% summarise(eff = sum({{ pond }})) %>% ungroup
    if (na.rm) ct <- na.omit(ct)
    ct <- ct %>% mutate_if(is.factor, as.character)
    if (marge == 0 & ! abs) ct <- ct %>% mutate(eff = eff / sum(eff))
    if (marge == 1) ct <- ct %>% group_by({{ x1 }}) %>% mutate(eff = eff / sum(eff))
    if (marge == 2) ct <- ct %>% group_by({{ x2 }}) %>% mutate(eff = eff / sum(eff))
    if (total){
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
    }
    ct <- ct %>% pivot_wider(names_from = {{ x2 }}, values_from = eff)
    if (total){
        if (marge == 2) ct <- select(ct, - Total)
        if (marge == 1) ct <- filter(ct, {{ x1 }} != "Total" | is.na({{ x1 }}))
    }
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


#' Indices synthétiques
#'
#' Calculs d'indices synthétiques (Laspeyres, Paasche et Fisher),
#' chaînés ou non.
#'
#' 
#' @name indices
#' @aliases indices
#' @param data un tibble
#' @param an la date d'observation
#' @param bien le bien considéré
#' @param quant la quantité
#' @param prix le prix
#' @param base l'année de base
#' @param chaine si vrai, la formule de l'indice chaînée est utilisée
#' @return un tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows lag rename
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @author Yves Croissant
indices <- function(data, an, bien, quant, prix, base, chaine = FALSE){
    data <- data %>% select(an = {{ an }}, bien = {{ bien }}, quant = {{ quant }}, prix = {{ prix }})
    # data for the base year
    data_base <- data %>% filter(an == base) %>% select(- an) %>%
        rename(quant_base = quant, prix_base = prix)
    dep_tot_base <- data_base %>% summarise(depense = sum(quant_base * prix_base))
    data_base <- data_base %>% bind_cols(dep_tot_base) %>%
        mutate(cbudg_base = quant_base * prix_base / depense) %>%
        select(- depense)
    # total expense by year
    dep_tot <- data %>% group_by(an) %>% summarise(dep_tot = sum(prix * quant))
    # initial data set with budget coefficients
    data <- data %>% left_join(dep_tot) %>% mutate(cbudg = quant * prix / dep_tot) %>%
        select(- dep_tot)
    data <- data %>% left_join(data_base)
    if (! chaine){
        data_synth <- data %>% mutate(prix = prix / prix_base,
                                      quant = quant / quant_base) %>%
            select(- quant_base, - prix_base)
        data_synth <- data_synth %>% group_by(an) %>%
            summarise(laspeyres_prix = sum(prix * cbudg_base) * 100,
                      laspeyres_quant = sum(quant * cbudg_base) * 100,
                      pasche_prix = 1 / sum(1 / prix * cbudg) * 100,
                      pasche_quant = 1 / sum(1 / quant * cbudg) * 100) %>%
            mutate(fish_prix = sqrt(laspeyres_prix * pasche_prix),
                   fish_quant = sqrt(laspeyres_quant * pasche_quant))
        data_synth <- data_synth %>% pivot_longer(-an) %>%
            separate(name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = grandeur, values_from = value)
    }
    else{
        data_synth <- data %>% group_by(bien) %>%
            mutate(prix = prix / lag(prix),
                   quant = quant / lag(quant),
                   lcbudg = lag(cbudg)) %>% group_by(an) %>%
            summarise(laspeyres_prix = sum(prix * lcbudg),
                      laspeyres_prix = ifelse(is.na(laspeyres_prix), 1, laspeyres_prix),
                      laspeyres_quant = sum(quant * lcbudg),
                      laspeyres_quant = ifelse(is.na(laspeyres_quant), 1, laspeyres_quant),
                      pasche_prix = sum(prix * cbudg),
                      pasche_prix = ifelse(is.na(pasche_prix), 1, pasche_prix),
                      pasche_quant = sum(quant * cbudg),
                      pasche_quant = ifelse(is.na(pasche_quant), 1, pasche_quant)) %>%
            mutate(laspeyres_prix = cumprod(laspeyres_prix),
                   laspeyres_quant = cumprod(laspeyres_quant),
                   pasche_prix = cumprod(pasche_prix),
                   pasche_quant = cumprod(pasche_quant))
        data_synth <- data_synth %>% pivot_longer(- an) %>%
            separate(name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = grandeur, values_from = value)
        data_base <- filter(data_synth, an == base) %>%
            rename(prix_base = prix, quant_base = quant) %>% select(-an)
        data_synth <- data_synth %>% left_join(data_base) %>%
            mutate(prix = prix / prix_base * 100,
                   quant = quant / quant_base * 100) %>%
            select(- prix_base, - quant_base)
    }
    data_synth
}

#' Indicateurs de tendance centrale
#'
#' Calcule les trois indicateurs de tendance centrale (moyenne,
#' médiane et mode), ainsi que les densités correspondantes
#'
#' 
#' @name central
#' @aliases central
#' @param data un tibble
#' @param x la variable considérée (nécessairement numérique)
#' @param breaks un vecteur de limites de classes
#' @return un tibble contenant trois variables, `name`, `x` et `y`
#' @export
#' @author Yves Croissant
central <- function(data, x, breaks){
    mu <- data %>% pull({{ x }}) %>% mean
    Me <- data %>% pull({{ x }}) %>% median
    ra <- data %>% mutate(xcl = cut({{ x }}, breaks)) %>%
        freq_table(xcl, total = FALSE) %>%
        bind_cols(a = diff(breaks)) %>%
        mutate(y = eff / a) %>%
        separate(xcl, into = c("deb", "fin"), sep = ",", remove = FALSE) %>%
        mutate(deb = substr(deb, 2, nchar(deb)),
               fin = substr(fin, 1, nchar(fin) - 1),
               deb = as.numeric(deb),
               fin = as.numeric(fin),
               x = (deb + fin) / 2) %>%
        select(- a, - eff)
    mode <- ra %>% filter(y == max(y)) %>% mutate(name = "mode") %>% select(name, x, y)
    mu2 <- ra %>% filter(mu > deb, mu <= fin) %>% mutate(x = mu, name = "moyenne") %>% select(name, x, y)
    Me2 <- ra %>% filter(Me > deb, Me <= fin) %>% mutate(x = Me, name = "médiane") %>% select(name, x, y)
    mu2 %>% add_row(Me2) %>% add_row(mode)
}
