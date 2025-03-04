% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fonction.R
\name{calcul_distribution_age}
\alias{calcul_distribution_age}
\title{Calculer la distribution des ages}
\usage{
calcul_distribution_age(data)
}
\arguments{
\item{data}{Un dataframe contenant au moins la colonne suivante :
\itemize{
\item \code{Date.de.naissance} (Date) : La date de naissance des elus, qui doit etre deja convertie en format \code{Date}.
}}
}
\value{
Un vecteur numerique representant les ages des elus en annees.
}
\description{
Cette fonction calcule la distribution des ages a partir d'une colonne \code{Date.de.naissance} en format \strong{Date}.
}
\details{
\itemize{
\item La fonction verifie la presence de la colonne \code{Date.de.naissance}.
\item Elle verifie que la colonne est bien de type \strong{Date}, sinon elle renvoie une erreur.
\item Elle calcule l'age en annees en utilisant la difference entre la date du jour (\code{Sys.Date()}) et la \code{Date.de.naissance}.
}
}
