#' Indices synthétiques
#'
#' Calcul des indices synthétiques (Laspeyres, Paasche and Fisher),
#' chaînés ou non
#' 
#' @name indexes
#' @aliases indexes
#' @param data un tibble
#' @param an la série contenant les dates
#' @param bien la série contenant les biens 
#' @param quant la série contenant les quantités
#' @param prix la série contenant les prix
#' @param base l'année de base de l'index
#' @param chaine si fixé à `TRUE`, un indice chaîné est calculé
#' @return un tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows lag rename left_join `%>%` filter mutate select
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @author Yves Croissant
indexes <- function(data, an, bien, quant, prix, base, chaine = FALSE){
    data <- data %>% select(an = {{ an }}, bien = {{ bien }}, quant = {{ quant }}, prix = {{ prix }})
    # data for the base year
    data_base <- data %>% filter(an == base) %>% select(- an) %>%
        rename(quant_base = quant, prix_base = prix)
    dep_tot_base <- data_base %>% summarise(depense = sum(.data$quant_base * .data$prix_base))
    data_base <- data_base %>% bind_cols(dep_tot_base) %>%
        mutate(cbudg_base = .data$quant_base * .data$prix_base / .data$depense) %>%
        select(- .data$depense)
    # total expense by year
    dep_tot <- data %>% group_by(an) %>% summarise(dep_tot = sum(prix * quant))
    # initial data set with budget coefficients
    data <- data %>% left_join(dep_tot) %>% mutate(cbudg = quant * prix / dep_tot) %>%
        select(- dep_tot)
    data <- data %>% left_join(data_base)
    if (! chaine){
        data_synth <- data %>% mutate(prix = prix / .data$prix_base,
                                      quant = quant / .data$quant_base) %>%
            select(- .data$quant_base, - .data$prix_base)
        data_synth <- data_synth %>% group_by(an) %>%
            summarise(Laspeyres_prix = sum(prix * .data$cbudg_base) * 100,
                      Laspeyres_quant = sum(quant * .data$cbudg_base) * 100,
                      Paasche_prix = 1 / sum(1 / prix * .data$cbudg) * 100,
                      Paasche_quant = 1 / sum(1 / quant * .data$cbudg) * 100) %>%
            mutate(Fisher_prix = sqrt(.data$Laspeyres_prix * .data$Paasche_prix),
                   Fisher_quant = sqrt(.data$Laspeyres_quant * .data$Paasche_quant))
        data_synth <- data_synth %>% pivot_longer(-an) %>%
            separate(.data$name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = .data$grandeur, values_from = .data$value)
    }
    else{
        data_synth <- data %>% group_by(bien) %>%
            mutate(prix = prix / lag(prix),
                   quant = quant / lag(quant),
                   lcbudg = lag(.data$cbudg)) %>% group_by(an) %>%
            summarise(Laspeyres_prix = sum(prix * .data$lcbudg),
                      Laspeyres_prix = ifelse(is.na(.data$Laspeyres_prix), 1, .data$Laspeyres_prix),
                      Laspeyres_quant = sum(quant * .data$lcbudg),
                      Laspeyres_quant = ifelse(is.na(.data$Laspeyres_quant), 1, .data$Laspeyres_quant),
                      Paasche_prix = sum(prix * .data$cbudg),
                      Paasche_prix = ifelse(is.na(.data$Paasche_prix), 1, .data$Paasche_prix),
                      Paasche_quant = sum(quant * .data$cbudg),
                      Paasche_quant = ifelse(is.na(.data$Paasche_quant), 1, .data$Paasche_quant)) %>%
            mutate(Laspeyres_prix = cumprod(.data$Laspeyres_prix),
                   Laspeyres_quant = cumprod(.data$Laspeyres_quant),
                   Paasche_prix = cumprod(.data$Paasche_prix),
                   Paasche_quant = cumprod(.data$Paasche_quant))
        data_synth <- data_synth %>% pivot_longer(- an) %>%
            separate(.data$name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = .data$grandeur, values_from = .data$value)
        data_base <- filter(data_synth, an == base) %>%
            rename(prix_base = prix, quant_base = quant) %>% select(-an)
        data_synth <- data_synth %>% left_join(data_base) %>%
            mutate(prix = prix / .data$prix_base * 100,
                   quant = quant / .data$quant_base * 100) %>%
            select(- .data$prix_base, - .data$quant_base)
    }
    data_synth
}
