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
#' data hub: Height
#' 
#' [Universitat Tubingen](https://uni-tuebingen.de/fakultaeten/wirtschafts-und-sozialwissenschaftliche-fakultaet/faecher/fachbereich-wirtschaftswissenschaft/wirtschaftswissenschaft/lehrstuehle/volkswirtschaftslehre/wirtschaftsgeschichte/forschung/data-hub-height/)
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
#' Journal of Applied Econometrics [data archive](https://qed.econ.queensu.ca/jae/)
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
#' Base comparateur de territoires
#'
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/2521169)
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
#' site de l'[American Economic Association](https://www.aeaweb.org/articles?id=10.1257/aer.100.5.2548)
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
#' Site de la revue [Data in Brief](https://www.sciencedirect.com/science/article/pii/S2352340918315191#ec0006)
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
#' @name Emploi
#' @docType data
#' @format Un tibble contenant
#'
#' - activité : une variable catégorielle permettant de distinguer les
#' inactifs `inactif, les chômeurs `chomeur` et les actifs occupés
#' `occupé`,
#' - temps : temps de travail, `neant`, `partiel`, `complet` et `inconnu`,
#' - diplome : niveau de diplome,
#' - age : l'âge en tranche,
#' - sexe : le sexe (`F` ou `M`),
#' - menage : type de ménage, une personne `unepers`, famille
#' monoparentale `monop`, couple sans enfants `cpl`, couple avec
#' enfants `cplenf` et autres ménages `autre`,
#' - ponderations : pondération pour chaque observation permettant de
#' passer de l'enquête à la population,
#' - durchom : durée de l'épisode de chômage, en tranche,
#' - durinact : durée de l'épisode d'inactivité, en tranche, 
#' - durentr : durée de travail dans l'entreprise, en tranche,
#' - horaires : nombre d'heures de travail hebdomadaire, en tranche d'heures,
#' - statut : variable catégorielle intégrant les deux variables `activite` et `temps`, soit `inactif`, `chomeur`, `partiel`, `complet` et `inconnu`
#'
#' @source
#' Activité, emploi et chômage en 2018
#'
#' Enquête emploi en continu - Fichier détail
#'
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/4191029)
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
#' - secteur : secteur d'activité, `industrie`,
#' `construction`, `commerce`, `services` et `administration`,
#' - age :  l'âge du salarié en année,
#' - dept : code du département d'emploi,
#' - heures : nombre d'heures annuelles travaillées,
#' - sexe : sexe du salarié, `homme` ou `femme`,
#' - salaire : tranches de salaire, en milliers d'euros par an,
#' - taille : tranche d'effectif de l'entreprise
#'
#' @source
#' Description des emplois privés et publics et des salaires en 2015
#'
#' DADS grand format 2015 - Fichiers détail
#'
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/3536754)
#'
NULL

#' Les décès en 2017
#'
#' Les fichiers de l'Etat-Civil recense de manière exhaustive les
#' naissances, les décès et les mariages. 
#'
#' @name Deces
#' @docType data
#' @format Un tibble contenant :
#' - anais : année de naissance
#' - depnais : département de naissance
#' - depdom : département de domicile
#' - sexe : `H` pour un homme, `F` pour une femme
#' - activ : indicateur d'activité, `retr` pour retraités, `inact`
#' pour autres inactifs et `actif` pour les actifs (y compris les
#' demandeurs d'emploi)
#' - nat : indicateur de la nationalité, soit `fr` pour les français
#' et `etr` pour les étrangers
#' - mat : indicateur de l'état matrimonial, `celib` pour un
#' célibataire, `marie` pour une personne mariée, `veuf` pour un veuf
#' et `divorc` pour une personne divorcée
#' - mdec : mois de naissance
#' - lieu : lieu de naissance, `hop` pour établissement hospitalier,
#' `mretr` pour maison de retraite, `log` pour dominicile et `autre`
#' pour autre
#'
#' @source
#' Naissances, décès et mariages en 2017
#'
#' Etat civil - Fichiers détail
#'
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/3596190?sommaire=3596198)
NULL


#' Les naissances en 2017
#'
#' Les fichiers de l'Etat-Civil recense de manière exhaustive les
#' naissances, les décès et les mariages. 
#'
#' @name Naissances
#' @docType data
#' @format Un tibble contenant :
#' - département : le code du département de naissance,
#' - sexe : le sexe de l'enfant,
#' - mois : le mois de la naissance,
#' - age : l'âge de la mère,
#' - tranche : tranche d'âge de la mère,
#' - trimestre : trimestre de naissance
#'
#' @source
#' Naissances, décès et mariages en 2017,
#'
#' Etat civil - Fichiers détail
#'
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/3596190?sommaire=3596198)
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
#' Site de la revue [Data in Brief](https://www.sciencedirect.com/science/article/pii/S2352340919310522#tbl5)
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
#' Consommation régionale, journalière et définitive de gaz des distributions publiques à l'interface avec GRTgaz et Terega,
#'
#' Open Data Réseaux Energies
#' 
#' site [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/consommation-journaliere-et-horaire-de-gaz-des-clients-grd-et-eld-par-region-reseaux-grtgaz-terega-donnees-definitives-1/)
NULL

#' Pêche en France
#'
#' Données hebdomadaires sur la pêche en France pour différents types de poissons
#'
#' @name Poissons
#' @docType data
#' @format Un tibble contenant :
#' - poisson : espèce de poisson
#' - date : date de début de la semaine d'observation
#' - poids : quantité de merlan pêchée en kg
#' - valeur : valeur des ventes de merlan
#'
#' @source
#'
#' Série hebdomadaire par espèce (produits de la mer)
#'
#' FranceAgriMer
#' 
#' site [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/serie-hebdomadaire-par-espece-produits-de-la-mer/)
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
#' - price : prix du logement en milliers d'euros
#'
#' @source
#' Site de la revue [Data in Brief](https://www.sciencedirect.com/science/article/pii/S2352340915003224)
#' @references
#' \insertRef{BONI:COPI:15}{statecoaes}
#' 
NULL

#' Transport aérien en Europe
#'
#' Ce jeux de données indique pour l'Italie le nombre de vols et le
#' nombre de passagers par mois
#'
#' @name Vols
#' @docType data
#' @format Un tibble contenant :
#' - date : la date d'observation (premier jour du mois),
#' - vols : nombe de vols
#' - passagers : nombre de passagers transportés
#'
#' @source
#'
#' Transport aérien de passagers par pays déclarant (avia_paoc)
#' 
#' site d'[Eurostat](https://ec.europa.eu/eurostat/fr/)
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
#' Consomation des ménages en 2019
#'
#' Comptes nationaux annuels - base 2014
#' 
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/4494154?sommaire=4494218)
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
#' Valeurs immobilières, économiques et financières de 1800 à 2015
#'
#' Ministère de la transition écologique et solidaire
#' 
#' site [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/valeurs-immobilieres-economiques-et-financieres-de-1800-a-2015/)
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
#' site de [Pôle emploi](https://statistiques.pole-emploi.org/stmt/static/methode_2018)
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
#'
#' Tableau de bord tabac
#'
#' Observatoire français des drogues et des toxicomanies
#'
#' site de l'[ofdt](https://www.ofdt.fr/statistiques-et-infographie/tableau-de-bord-tabac/)
#'
NULL


#' Prénoms des enfants nés en France
#'
#' Données annuelles indiquant, de 1900 à 2019, le nombre d'enfant
#' ayant reçu un prénom
#'
#' @name Prenoms
#' @docType data
#' @format Un tibble de 601461 observations contenant :
#' - sexe : une variable catégorielle avec pour modalités `homme` et `femme`,
#' - prenom : le prénom considéré,
#' - annee : l'année considérée,
#' - nombre : le nombre d'enfants ayant reçu le prénom considéré pour l'année considérée
#' 
#' @source
#'
#' Les prénoms en 2019
#'
#' Etat civil
#' 
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/2540004?sommaire=4767262)
#'
NULL

#' Données du recensement pour le département de la Seine et Marne (78)
#'
#' Différentes informations concernant les familles en Seine et Marne,
#' issues du recensement de la population de 2017
#' 
#' @name RGP77
#' @docType data
#' @format Un tibble de 101,987 observations contenant :
#' - voitures : le nombre de voitures
#' - pieces : le nombre de pièces du logement
#' - enfants : le nombre d'enfants,
#' - typemen : le type de famille : couple (`couple`) ou famille
#' mono-parentale (`monop`),
#' - ER : le sexe du Rième enfant (de 1 à 11).
#' 
#' @source
#'
#' Logements, individus, activité, mobilités scolaires et professionnelles, migrations résidentielles en 2017
#'
#' Recensement de la population - Fichier détail
#'
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/4507685?sommaire=4508161)
#'
NULL


#' Relation entre richesse et développement
#'
#' Données sur l'espérance de vie et le PIB par tête, qui permettent
#' d'analyser la relation entre richesse et développement
#' 
#' @name Deaton
#' @docType data
#' @format Un tibble de 179 observations contenant :
#' - code : le code du pays
#' - nom : le nom du pays
#' - region : la région du monde dans laquelle est située le pays
#' - incgp : le groupe de revenu du pays
#' - gdp : le PIB par tête
#' - pop : la population
#' - le : l'espérance de vie
#' - subnom : un sous-ensemble de 17 noms de pays
#' 
#' @source
#' site de la [Banque Mondiale](https://www.https://databank.worldbank.org/home.aspx)
#'
#' @references
#' \insertRef{DEAT:16}{statecoaes}
NULL

#' Ventes de voitures d'occasion
#'
#' Données sur des voitures d'occasion aux Etats-Unis vendues sur le site Ebay
#'
#' @name Ebay
#' @docType data
#' @format Un tibble contenant :
#' - miles : le kilométrage,
#' - bid : le montant de l'enchère gagnante,
#' - year : l'année de mise en service de la voiture
#'
#' @source
#' Journal of Applied Econometrics [data archive](https://qed.econ.queensu.ca/jae/)
#' @references
#' \insertRef{LEWI:11}{statecoaes}
NULL


#' Nombre de pièces des logements
#'
#' Table de fréquence indiquant le nombre de pièce par logements
#'
#' @name Logements
#' 
#' @docType data
#' @format Un tibble contenant :
#' - pieces : le nombre de pièces du logement,
#' - nombre : le nombre de logements (en millions) pour chaque nombre
#' de pièces.
#'
#' @source
#'
#' Logements et résidences principales en 2017,
#'
#' Recensement de la population
#'
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/4515537?sommaire=4516107#consulter).
NULL

#' Nombre d'entreprises par secteur et par taille
#'
#' Table de contingence indiquant le nombre d'entreprises par tranche
#' de taille et par secteur d'activité
#'
#' @name Clap
#'
#' @docType data
#' @format Un tibble contenant :
#' - secteur le secteur d'activité
#' - taille la tranche de taille de l'entreprise mesurée par le nombre de salariés,
#' - nombre le nombre d'établissements,
#' - salaries le nombre de salariés
#' 
#' @source
#' 
#' **Clap** (Connaissance locale de l'appareil productif)
#' 
#' site de l'[INSEE](https://www.insee.fr/fr/statistiques/2021289)
NULL

