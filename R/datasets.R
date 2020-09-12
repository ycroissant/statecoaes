#' La taille des conscrits
#'
#' Un échantillon de 21064 conscrits en bavarois au XIXeme siècle
#'
#' @name Conscrits
#' @docType data
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
#' Comportement hebdomadaire d'achat de Ketchup par 3088 ménages au cours de l'année 1987
#'
#' @name Ketchup
#' @docType data
#' @format Un tibble contenant
#'
#' - chid: identifiant de l'achat
#' - id : identifiant du ménage
#' - week : identifiant de la semaine
#' - choice : 1 si du ketchup a été acheté, 0 autrement
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

