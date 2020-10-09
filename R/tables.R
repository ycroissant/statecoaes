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
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows `%>%` n
#' @importFrom stats na.omit
#' @importFrom rlang `:=`
#' @author Yves Croissant
#' @examples
#'
#' freq_table(Emploi, activité, "n")
#' freq_table(Emploi, activité, "nN")
#' freq_table(Emploi, activité, "fF", weights = pondérations)
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
    max <- attr(x, "max")
    total <- attr(x, "total")
    nr <- nrow(x)
    if (total | ! is.na(max)) x[[1]] <- as.character(x[[1]])
    if (total) x[[1]][nr] <- "Total"
    if (! is.na(max)) x[[1]][nr - total] <- paste(">= ", max, sep ="")
    class(x) <- class(x)[- 1]
    format(x, ..., n = n, width = width, n_extra = n_extra)
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
#' @importFrom stats median
#' @export
#' @author Yves Croissant
central <- function(data, x, breaks){
    mu <- data %>% pull({{ x }}) %>% mean
    Me <- data %>% pull({{ x }}) %>% median
    ra <- data %>% mutate(xcl = cut({{ x }}, breaks)) %>%
        freq_table(xcl, "f", total = FALSE) %>%
        bind_cols(a = diff(breaks)) %>%
        mutate(y = f / a) %>%
        separate(xcl, into = c("deb", "fin"), sep = ",", remove = FALSE) %>%
        mutate(deb = substr(deb, 2, nchar(deb)),
               fin = substr(fin, 1, nchar(fin) - 1),
               deb = as.numeric(deb),
               fin = as.numeric(fin),
               x = (deb + fin) / 2) %>%
        select(- a, - f)
    mode <- ra %>% filter(y == max(y)) %>% mutate(name = "mode") %>% select(name, x, y)
    mu2 <- ra %>% filter(mu > deb, mu <= fin) %>% mutate(x = mu, name = "moyenne") %>% select(name, x, y)
    Me2 <- ra %>% filter(Me > deb, Me <= fin) %>% mutate(x = Me, name = "médiane") %>% select(name, x, y)
    mu2 %>% add_row(Me2) %>% add_row(mode)
}



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
#' @param first une valeur numérique indiquant le centre de la
#'     première classe
#' @param last une valeur numérique indiquant le centre de la dernière
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
                       first = NULL, last = NULL, right = NULL,
                       total = FALSE, inflate = 1){
    # check wether the computation of densities is required and if so
    # create a boolean and remove d from cols
    cols_vec <- strsplit(cols, "")[[1]]
    remove_counts <- FALSE
    if ("d" %in% cols_vec){
        compute_densities <- TRUE
        cols_vec <- setdiff(cols_vec, "d")
        if (! "n" %in% cols_vec){
            cols_vec <- c("n", cols_vec)
            remove_counts <- TRUE
        }
        cols <- paste(cols_vec, collapse = "")
        if (compute_densities) cols_vec <- c(cols_vec, "d")
        if (remove_counts) cols_vec <- setdiff(cols_vec, "n")
    }
    else compute_densities <- FALSE
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
        res <- data %>% mutate("{{ x }}" := cut({{ x }}, breaks, right = right)) %>%
            freq_table({{ x }}, cols = cols, total = total)
    }
    else{
        # x is a class, return an error if the right argument is provided
        if (! is.null(right))
            stop("the right argument is irrelevant in this context as classes are provided")
        if (! is.null(breaks)){
            # x breaks are provided in order to reduce the number of classes
            # first guess the value of right
            left_op <- data %>% slice(2) %>% pull({{ x }}) %>% substr(1, 1)
            if (left_op == "[") right <- FALSE else right <- TRUE
            # get the initial classes and computs the breaks
            init_cls <- data %>% pull({{ x }}) %>% unique %>% as.character
            lbond <- cls2val(init_cls, 0)
            ubond <- cls2val(init_cls, 1)
            cls_table <- tibble("{{ x }}" := init_cls, lbond, ubond) %>% arrange(lbond)
            init_bks <- sort(union(lbond, ubond))
            cls_table <- cls_table %>% mutate(center = cls2val({{ x }}, 0.5))
            # min/max values of the new breaks lower/larger than the
            # min/max values of the initial breaks are not allowed
            if (min(breaks) < min(init_bks)) stop("the minimal value provided is lower than the initial lower bond")
            if (max(breaks) > max(init_bks)) stop("the minimal value provided is lower than the initial lower bond")
            # min/max values of the initial breaks are included in the
            # new breaks if necessary
            if (! min(init_bks) %in% breaks) breaks <- c(breaks, min(init_bks))
            if (! max(init_bks) %in% breaks) breaks <- c(breaks, max(init_bks))
            # put in form the vector of new breaks and check whether
            # some values are not part of the initial breaks
            breaks <- sort(unique(breaks))
            dbrks <- setdiff(breaks, init_bks)
            if (length(dbrks) > 0) stop(paste(paste(sort(dbrks), collapse = ", ")),
                                        paste(" are provided in the breaks argument but are ",
                                              "not part of the  initial set of breaks", sep = ""),
                                        sep = "")
            cls_table <- cls_table %>% mutate(new_cls = cut(center, breaks, right = right)) %>%
                select({{ x }}, new_cls)
            data <- data %>% left_join(cls_table) %>% select(- {{ x }}) %>%
                rename("{{ x }}" := new_cls)
        }
        res <- freq_table(data, {{ x }}, cols = cols, total = FALSE)
    }
    if ((any(c("x", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(x = cls2val({{ x }}, 0.5, first = first,
                                        last = last, inflate = inflate))
    if ((any(c("l", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(l = cls2val({{ x }}, 0))
    if ((any(c("u", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(u = cls2val({{ x }}, 1))
    if (("a" %in% vals_vec) | compute_densities){
        N <- nrow(res)
        last_inf <- res %>% slice(N) %>% pull(u) %>% is.infinite
        if (last_inf){
            res <- res %>% slice(N) %>% mutate(u2 = l + 2 * (x - l)) %>% pull(u2)
            res <- res %>% mutate(u2 = ifelse(is.infinite(u), u2, u))
            res <- res %>% mutate(a = u2 - l) %>% select(- u2)
        }
        else res <- res %>% mutate(a = u - l)
        if (! "l" %in% vals_vec) res <- res %>% select(- l)
        if (! "u" %in% vals_vec) res <- res %>% select(- u)
        if (compute_densities) res <- res %>% mutate(d = n / sum(n) / a)
        if (remove_counts) res <- res %>% select(- n)
        if (! "a" %in% vals_vec) res <- res %>% select(- a)
    }
    if (! "x" %in% vals_vec) res <- res %>% select(- x)
    cols_pos <- match(c("n", "f", "p", "N", "F", "P", "d"), names(res)) %>% na.omit %>% sort
    vals_pos <- match(c("x", "l", "a", "u"), names(res)) %>% na.omit %>% sort
    res %>% select({{ x }}, all_of(c(vals_pos, cols_pos)))
}

#' Convert class to values
#'
#' Convert a string (or factor) which represents a class to a value of
#' the underlying variable
#' 
#' @name cls2val
#' @aliases cls2val
#' @param x a series that contains a class of values, the first and
#'     last characters should be any of `[`, `(`, `]`, `)` and the
#'     other characters should be interpreted as two numerical values
#'     separated by a `,`
#' @param pos a numeric between 0 and 1, 0 for the lower bond, 1
#'     for the upper bond, 0.5 for the center of the class (and any
#'     other value between 0 and 1)
#' @param first center of the first class, if one wants to specifie
#'     something different from the average of the lower and the upper
#'     bonds
#' @param last the center of the last class, if one wants to specifie
#'     something different from the average of the lower and the upper
#'     bonds
#' @param inflate in the case where the upper bond is infinite and
#'     `last` is not provided, the upper bond of the last class is set
#'     to the lower bond of the last class and the range of the
#'     previous class times this coefficient (which default value is
#'     one)
#' @return a numerical vector
#' @export
#' @author Yves Croissant
#' @examples
#'
#' # salaire is a class of wage in the Salaires data set ; first
#' # extract unique values
#' sals <- Salaires %>% pull(salaire) %>% levels
#' # compute the lower bonds
#' sals %>% cls2val(0)
#' # lower bonds with a user specified center value for the first class
#' sals %>% cls2val(0, first = 0.18) %>% head
#' # compute the upper bonds
#' sals %>% cls2val(1)
#' # note that the Inf upper bond is replaced by 50 + (50 - 40), ie
#' # the lower bond plus the range of the previous class
#' sals %>% cls2val(1, last = 100) %>% tail
#' # last is provided (the center of the last class) and the upper
#' # bond is adapted accordingly, which means 50 + (100 - 50) * 2 =
#' # 150
#' sals %>% cls2val(1, inflate = 3) %>% tail
#' # inflate is provided, so that the range of the last class is three
#' # times the range of the previous one
cls2val <- function(x, pos = 0, first = NULL, last = NULL, inflate = NULL){
    K <- length(x)
    if (! is.null(last) & ! is.null(inflate)) stop("only one of last or inflate should be set")
    if (! is.numeric(pos)) stop("pos should be numeric")
    if (is.numeric(pos) & ! (pos >= 0 & pos <= 1)) stop("pos should be between 0 and 1")
    if (! is.integer(pos)){
        if (near(pos, 1L)) pos <- 1L
        if (near(pos, 0L)) pos <- 0L
    }
    x <- x %>% as.character %>% strsplit(",")
    xl <- sapply(x, function(x) x[1])
    xl <- as.numeric(substr(xl, 2, nchar(xl)))
    xu <- sapply(x, function(x) x[2])
    xu <- as.numeric(substr(xu, 1, nchar(xu) - 1))
    if (! is.null(first)){
        if (! (first >= xl[1] & first <= xu[1])) stop("irrelevant value for first")
        xl[1] <- first - (xu[1] - first)
    }
    if (! is.null(last)){
        if (! (last >= xl[K] & last <= xu[K])) stop("irrelevant value for last")
        xu[K] <- xl[K] + 2 * (last - xl[K])
    }
    else{
        if (is.infinite(xu[K])){
            if (is.null(inflate)) inflate <- 1
            xu[K] <- xl[K] + inflate * (xl[K]- xl[K - 1])
        }
    }
    x <- (1 - pos) * xl + pos * xu
    x
}

