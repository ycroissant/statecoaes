#' La taille des conscrits
#'
#' Un échantillon de 21064 conscrits en bavarois au XIXeme siècle
#'
#' @name Conscrits
#' @docType data
#' @import tidyverse
#' @format Un tibble contenant
#'
#' - taille: la taille du conscrits en centimètres
#'
#' @source
#' [data hub: Height Universitat Tubingen](https://uni-tuebingen.de/fakultaeten/wirtschafts-und-sozialwissenschaftliche-fakultaet/faecher/fachbereich-wirtschaftswissenschaft/wirtschaftswissenschaft/lehrstuehle/volkswirtschaftslehre/wirtschaftsgeschichte/forschung/data-hub-height/)
#' @references
#' \insertRef{BATE:MURR:00}{statecoaes}
#' @keywords datasets
#' @importFrom Rdpack reprompt

NULL


#' Achat de Ketchup
#'
#' Comportement hebdomadaire d'achat de Ketchup par 3088 ménages au cours des années 1986-88
#'
#' @name Ketchup
#' @docType data
#' @format Un tibble contenant
#'
#' - id : identifiant du ménage
#' - week : identifiant de la semaine
#' - year : l'année de l'achat
#' - quarter : le trimestre de l'achat
#' - choice : 1 si du ketchup a été acheté, 0 autrement
#' - buy : 1 si le ménage a acheté du Ketchup au moins une fois pendant l'ensemble de la période d'observation
#'
#'
#' @source
#' [Journal of Applied Econometrics data archive](https://qed.econ.queensu.ca/jae/)
#' @references
#' \insertRef{CHIN:ERDE:KEAN:09}{statecoaes}
#' @keywords datasets
#' @importFrom Rdpack reprompt

NULL


#' Population et surface des communes françaises
#'
#' Population et surface des 34989 communes françaises
#'
#' @name Communes
#' @docType data
#' @format Un tibble contenant
#'
#' - code : le code postal de la commune
#' - pop : la population en milliers d'habitants
#' - surface : la surface en kilomètres carré
#'
#' @source
#' [site de l'INSEE](https://www.insee.fr/fr/statistiques/2521169)
#'
#' @keywords datasets
#' @importFrom Rdpack reprompt

NULL


#' Séances de penalty
#'
#' Résultat de chaque tir au but (réussite ou échec) dans le cadre de
#' séances de penalty permettant de départager deux équipes à égalité
#'
#' @name Penalty
#' @docType data
#' @format Un tibble contenant
#'
#' - id : l'identifiant du match
#' - order : `first` pour l'équipe qui débute la séance de tirs aux
#' buts, `second` pour l'autre équipe
#' - rank : le rang du tir pour chaque équipe, 1 pour le premier
#' tireur, 2 pour le deuxième, etc.
#' - cumrank : rang cumulé pour le tir au but, 1 pour le premier tir
#' de l'équipe qui commence la séance, 2 pour le premier tir de
#' l'autre équipe, 3 pour le second tir de l'équipe qui commence, etc.
#'
#' - score : 1 en cas de réussite du tir au but, 0 autrement.
#'
#' @source
#' [site de l'AEA](https://www.aeaweb.org/articles?id=10.1257/aer.100.5.2548)
#' @references
#' \insertRef{APES:PALA:10}{statecoaes}
#' @keywords datasets
#' @importFrom Rdpack reprompt

NULL


#' File d'attente à la banque
#'
#' Mesure de la fille d'attente dans des banques au Nigéria, 38875
#' observations, pour 3 banques pendant 3 semaines
#'
#' @name Queue
#' @docType data
#' @format Un tibble contenant
#'
#' - id : l'identifiant de l'attente
#' - time : l'heure d'arrivée
#' - attente : la durée d'attente en minute
#' - opération : la durée de l'opération au guichet
#' - total : la temps total passé dans la banque
#' - bank : identifiant de la banque (A, B ou C)
#' - week : identitiant de la semaine (1, 2 ou 3)
#' - day : identifiant du jour de la semaine (monday, tuesday, ...)
#'
#' @source
#' [site de la revue Data in Brief](https://www.sciencedirect.com/science/article/pii/S2352340918315191#ec0006)
#' @references
#' \insertRef{NUNO:DEAL:NUNE:19}{statecoaes}
#' @keywords datasets
#' @importFrom Rdpack reprompt

NULL

#' Extraits de l'enquête emploi
#'
#' L'enquête emploi donne de nombreuses informations sur les
#' caractéristiques des emplois occupés par un échantillon de personne
#' (actif/non-actif, chômage, temps partiel, niveau de diplôme, etc.
#'
#' 
#' @name Emploi
#' @docType data
#' @format Un tibble contenant
#'
#' - activité : une variable catégorielle permettant de distinguer les
#' inactifs `inactif, les chômeurs `chômeur` et les actifs occupés
#' `occupé`.
#' - temps : temps de travail, `néant`, `partiel`, `complet` et `inconnu`.
#' - diplome : niveau de diplome
#' - âge : l'âge en années
#' - ménage : type de ménage, une personne `unepers`, famille
#' monoparentale `monop`, couple sans enfants `cpl`, couple avec
#' enfants `cplenf` et autres ménages `autre`
#' - pondérations : pondération pour chaque observation permettant de
#' passer de l'enquête à la population
#' - statut : variable catégorielle intégrant les deux variables `activité` et `temps`, soit `inactif`, `chômeur`, `partiel`, `complet` et `inconnu`
#'
#' @source Enquête emploi, site de l'[INSEE](www.insee.fr)
#'

NULL


#' Extraits de l'enquête DADS (Déclaration Annuelle des Données Sociales)
#'
#' L'enquête DADS indique de nombreuses caractéristiques des salariés,
#' en particulier le salaire annuel par tranche et le nombre d'heures
#' travaillées
#' 
#' @name Salaires
#' @docType data
#' @format Un tibble contenant :
#' 
#' - secteur : secteur d'activité, `agriculture`, `industrie`,
#' `construction`, `commerce`, `services` et `administration`,
#' - age :  l'âge du salarié en année,
#' - dept : code du département d'emploi,
#' - reg : code de la région d'emploi,
#' - heures : nombre d'heures annuelles travaillées,
#' - sexe : sexe du salarié, `homme` ou `femme`,
#' - salaire : tranches de salaire, en euros par an,
#' - sal_inf : limite inférieure de la tranche de salaire,
#' - sal_sup : limite supérieure de la tranche de salaire
#'
#' @source Enquête emploi, site de l'[INSEE](www.insee.fr)
#'

NULL

#' Les naissances en 2017
#'
#' Les fichiers de l'Etat-Civil recense de manière exhaustive les
#' naissances, les décès et les mariages. 
#'
#' @name Naissance
#' @docType data
#' @format Un tibble contenant :
#' - département : le code du département de naissance,
#' - sexe : le sexe de l'enfant,
#' - mois : le mois de la naissance,
#' - age : l'âge de la mère.
#'
#' @source Etat civil, site de l'[INSEE](www.insee.fr)
#' 

NULL
