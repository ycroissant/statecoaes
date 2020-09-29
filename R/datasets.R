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
#' travaillées (les données sont l'extrait de la base correspondant à
#' la région Ile de France
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


#' Locations saisonnières à Seville
#'
#' Prix et différentes caractéristiques de locations saisonnières dans
#' la ville de Séville, figurant sur le site bookings.com
#'
#' @name Seville
#' @docType data
#' @format Un tibble contenant :
#' - price : le prix pour deux nuits
#' - district : le disctrict dans lequel est situé la location
#' - dindex : un index de richesse du district (1 pour le plus riche),
#' - distance : distance aux centres d'intérêt majeurs de Seville
#' - rating : moyenne des notes obtenues précédemment sur le site
#' - beds : nombre de lits
#' - surface : surface de l'appartement
#' - parking : 1 si la location dispose d'un parking
#' - season : `ls_wd` pour saison creuse - jour de semaine, `ls_we`
#' pour saison creuse - week end, `hs_wd` pour haute saison - jour de
#' semaine, `hs_we` pour haute saison - week end, `holy_week` pour la
#' semaine sainte et `april_fair`
#' @source
#' [site de la revue Data in Brief](https://www.sciencedirect.com/science/article/pii/S2352340919310522#tbl5)
#' @references
#' \insertRef{SOLA:MARG:19}{statecoaes}

NULL

#' Consommation horaire de gaz
#'
#' Consommation par heure de gaz pour les différentes régions française
#'
#' @name Gaz
#' @docType data
#' @format Un tibble contenant :
#' - region : le code de la région
#' - heure : le jour et l'heure
#' - conso : la consommation
#'
#' @source
#' [site data.gouv.fr](https://www.data.gouv.fr/fr/datasets/consommation-journaliere-et-horaire-de-gaz-des-clients-grd-et-eld-par-region-reseaux-grtgaz-terega-donnees-definitives-1/)

NULL

#' Pêche au Merlan
#'
#' Pêche au Merlan (volume et valeur) par semaine pour les différents ports de pêche francais
#'
#' @name Merlan
#' @docType data
#' @format Un tibble contenant :
#' - debut : date de début de la semaine d'observation
#' - fin : date de la fin de la semaine d'observation
#' - poids : quantité de merlan pêchée en kg
#' - valeur : valeur des ventes de merlan
#' - id : identifiant du port
#' - nom : nom du port
#'
#' @source
#' [site data.gouv.fr](https://www.data.gouv.fr/fr/datasets/serie-hebdomadaire-par-espece-produits-de-la-mer/)
#'

NULL

#' Valeurs foncières à Padoue
#'
#' Valeurs des logements à Padoue
#'
#' @name Padoue
#' @docType data
#' @format Un tibble contenant :
#' - zone : une des 12 zones de la ville
#' - condition : `new` pour les logements neufs, `ordinary` ou `good` pour les logements anciens,
#' - house : variable indicatrice des maisons,
#' - rooms : nombre de pièces,
#' - bathrooms : nombre de salles de bain,
#' - parking : présence d'un parking,
#' - energy : catégorie d'énergie du logement (de A pour les plus
#' vertueux à G pour les plus dispensieux)
#' - surface : surface du logement en m$^2$
#' - price : prix du logement
#'
#' @source
#' [site de la revue Data in Brief](https://www.sciencedirect.com/science/article/pii/S2352340915003224)
#' @references
#' \insertRef{BONI:COPI:15}{statecoaes}
#' 

NULL

#' Transport aérien en Europe
#'
#' Ce jeux de données indique pour les différents pays de l'union
#' européenne le nombre de trajets en avion réalisés par trimestre
#'
#' @name Vols
#' @docType data
#' @format Un tibble contenant :
#' - date : la date d'observation (dernier jour du trimestre),
#' - an : l'année d'observation,
#' - trimestre : le trimestre d'observation
#' - pays : code du pays (2 caractères)
#' - passagers : nombre de passagers transportés
#'
#' @source
#' [site d'Eurostat](https://ec.europa.eu/eurostat/fr/)


NULL

#' Consommation en France
#'
#' Données annuelles de la consommation des ménages en France,
#' nomenclature par fonctions (alimentation, habillement, etc.)
#'
#' @name Consommation
#' @docType data
#' @format Un tibble contenant :
#' - an : l'année d'observation
#' - bien : un poste de consommation
#' - depense : dépense de consommation en euros courants,
#' - prix : indice de prix (base 100 en 2014),
#' Les postes de consommation sont :
#' - alimentation,
#' - alcool et tabac,
#' - habillement,
#' - logement,
#' - ammeublement,
#' - santé,
#' - transport,
#' - communications,
#' - loisirs,
#' - éducation,
#' - restauration,
#' - services divers,
#' - solde territorial
#'
#' @source
#' Consomation des ménages en 2019, comptes nationaux annuels - base 2014
#' 
#' [site de l'INSEE](https://www.insee.fr/fr/statistiques/4494154?sommaire=4494218)

NULL

#' Données de long terme pour la France
#'
#' Données macro-économiques annuelles pour la France de 1840 à 2015
#'
#' @name FranceLT
#' @docType data
#' @format Un tibble contenant :
#' - an : l'année d'observation,
#' - loyer : indice des loyers parisiens, base 100 en 2000,
#' - cpi : indice de prix à la consommation, base 100 en 2000,
#' - pib : produit intérieur brut en valeur,
#' - pop : population de la France,
#' - or : prix de l'or, base 100 en 2000
#' - tict : taux d'intérêt à court terme,
#' - tilt : taux d'intérêt à long terme.
#'
#' @source
#' Ministère de la transition écologique et solidaire
#' [site de data.gouv.fr](https://www.data.gouv.fr/fr/datasets/valeurs-immobilieres-economiques-et-financieres-de-1800-a-2015/)

NULL 

#' Demandeurs d'emploi inscrits à Pôle emploi
#'
#' Données mensuelles, par tranche d'âge et par sexe des demandeurs
#' d'emploi inscrits à Pôle emploi
#'
#' @name Chomage
#' @docType data
#' @format Un tibble de 2360 observations contenant :
#' - date : le mois d'observation, de janvier 1996 à juillet 2020
#' - sexe :  `homme`, `femme` ou `total`
#' - age : la tranche d'âge, `< 25`, `25-49`, `>= 50` ou `total`
#' 
#' @source
#' [site de Pôle emploi](https://statistiques.pole-emploi.org/stmt/static/methode_2018)
#'

NULL


#' Consommation de tabac
#'
#' Données annuelles concernant le prix unitaire et la quantité vendue
#' de cigarettes et de tabac à rouler
#'
#' @name Tabac
#' @docType data
#' @format Un tibble de 32 observations contenant :
#' - an : l'année d'obsrevation, de 2004 à 2019
#' - bien : une variable catégorielle, `cigarette` ou `tabac` (paquet de tabac à rouler),
#' - quant : millions de paquets vendus
#' - prix : prix du paquet en euros
#' 
#' @source
#' [Observatoire français des drogues et des toxicomanies](https://www.ofdt.fr/statistiques-et-infographie/tableau-de-bord-tabac/)
#'

NULL
