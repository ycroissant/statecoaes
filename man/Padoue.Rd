% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{Padoue}
\alias{Padoue}
\title{Valeurs foncières à Padoue}
\format{
Un tibble contenant :
\itemize{
\item zone : une des 12 zones de la ville
\item condition : \code{new} pour les logements neufs, \code{ordinary} ou \code{good} pour les logements anciens,
\item house : variable indicatrice des maisons,
\item rooms : nombre de pièces,
\item bathrooms : nombre de salles de bain,
\item parking : présence d'un parking,
\item energy : catégorie d'énergie du logement (de A pour les plus
vertueux à G pour les plus dispensieux)
\item surface : surface du logement en m$^2$
\item price : prix du logement en milliers d'euros
}
}
\source{
Site de la revue \href{https://www.sciencedirect.com/science/article/pii/S2352340915003224}{Data in Brief}
}
\description{
Valeurs des logements à Padoue
}
\references{
\insertRef{BONI:COPI:15}{statecoaes}
}
