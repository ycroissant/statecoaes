% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{Deces}
\alias{Deces}
\title{Les décès en 2017}
\format{
Un tibble contenant :
\itemize{
\item anais : année de naissance
\item depnais : département de naissance
\item depdom : département de domicile
\item sexe : \code{H} pour un homme, \code{F} pour une femme
\item activ : indicateur d'activité, \code{retr} pour retraités, \code{inact}
pour autres inactifs et \code{actif} pour les actifs (y compris les
demandeurs d'emploi)
\item nat : indicateur de la nationalité, soit \code{fr} pour les français
et \code{etr} pour les étrangers
\item mat : indicateur de l'état matrimonial, \code{celib} pour un
célibataire, \code{marie} pour une personne mariée, \code{veuf} pour un veuf
et \code{divorc} pour une personne divorcée
\item mdec : mois de naissance
\item lieu : lieu de naissance, \code{hop} pour établissement hospitalier,
\code{mretr} pour maison de retraite, \code{log} pour dominicile et \code{autre}
pour autre
}
}
\source{
Naissances, décès et mariages en 2017

Etat civil - Fichiers détail

site de l'\href{https://www.insee.fr/fr/statistiques/3596190?sommaire=3596198}{INSEE}
}
\description{
Les fichiers de l'Etat-Civil recense de manière exhaustive les
naissances, les décès et les mariages.
}
