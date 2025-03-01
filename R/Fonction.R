# --------------- Fonction 1 : Compter le nombre d'adjoints ---------------
#' Compter le nombre d'adjoints
#'
#' Compter le nombre d'adjoints uniques
#'
#' Cette fonction compte le nombre total d'adjoints presents dans un dataframe contenant leurs informations.
#'
#' @param data Un dataframe contenant au moins les colonnes `Nom.de.l.elu`, `Prenom.de.l.elu` et `Date.de.naissance`.
#'
#' @return Un entier representant le nombre total d'adjoints dans le dataframe.
#'
#' @details
#' - La fonction verifie la presence des colonnes obligatoires dans `data`.
#' - Elle compte le nombre total de lignes du dataframe, supposant que chaque ligne represente un elu unique.
#'
compter_nombre_d_adjoints <- function(data) {
  if (!all(c("Nom.de.l.elu", "Prenom.de.l.elu", "Date.de.naissance") %in% colnames(data))) {
    stop("Les colonnes de nom, prenom et date de naissance doivent etre presentes dans le dataframe.")
  }

  nombre_unique <- nrow(data)

  return(nombre_unique)
}

# --------------- Fonction 3 : calcul_distribution_age ---------------
#' Calculer la distribution des ages
#'
#' Cette fonction calcule la distribution des ages a partir d'une colonne `Date.de.naissance` en format **Date**.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Date.de.naissance` (Date) : La date de naissance des elus, qui doit etre deja convertie en format `Date`.
#'
#' @return Un vecteur numerique representant les ages des elus en annees.
#'
#' @details
#' - La fonction verifie la presence de la colonne `Date.de.naissance`.
#' - Elle verifie que la colonne est bien de type **Date**, sinon elle renvoie une erreur.
#' - Elle calcule l'age en annees en utilisant la difference entre la date du jour (`Sys.Date()`) et la `Date.de.naissance`.
#'
#'
#'
calcul_distribution_age <- function(data) {
  if (!"Date.de.naissance" %in% colnames(data)) {
    stop("La colonne 'Date.de.naissance' doit etre presente dans le dataframe.")
  }

  # Verification que la colonne "Date.de.naissance" est de type Date
  if (!inherits(data$Date.de.naissance, "Date")) {
    stop("La colonne 'Date.de.naissance' doit etre au format Date. Utilisez as.Date() pour la convertir.")
  }

  # Calcul des ages en annees
  ages <- as.numeric(difftime(Sys.Date(), data$Date.de.naissance, units = "days")) %/% 365

  return(ages)
}




# --------------- Fonction 4 : plot_code_professions ---------------
#' Visualiser la répartition des catégories socio-professionnelles des maires
#' Visualiser la repartition des elus par code professionnel
#'
#' Cette fonction genere un graphique en barres horizontales representant le nombre d'elus par code professionnel.
#'
#' @description
#' La fonction compte le nombre d'elus pour chaque code professionnel (`Code.de.la.categorie.socio.professionnelle`),
#' filtre les codes n'ayant aucun elu, puis produit un graphique en barres classe par ordre decroissant.
#'
#' @param data Un dataframe contenant au moins la colonne suivante :
#'   - `Code.de.la.categorie.socio.professionnelle` (character) : Code professionnel des elus.
#'
#' @return Un graphique `ggplot2` representant la repartition des elus par code professionnel.
#'
#' @details
#' - Les valeurs a `n = 0` sont filtrees avant l'affichage du graphique.
#' - Le graphique est trie par nombre d'elus de maniere decroissante.
#' - Les barres sont colorees en bleu fonce pour une meilleure lisibilite.
#'
#' @importFrom dplyr count filter arrange
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal
#'
plot_code_professions <- function(data) {
  if (!"Code.de.la.categorie.socio.professionnelle" %in% colnames(data)) {
    stop("La colonne 'Code.de.la.categorie.socio.professionnelle' doit etre presente dans le dataframe.")
  }

  # Compter le nombre d'elus par code professionnel et filtrer ceux a 0
  count_professions <- data |>
    dplyr::count(Code.de.la.categorie.socio.professionnelle) |>
    dplyr::filter(n > 0) |>
    dplyr::arrange(desc(n))

  # Generer le bar chart horizontal
  ggplot2::ggplot(count_professions, ggplot2::aes(x = n, y = reorder(Code.de.la.categorie.socio.professionnelle, n))) +
    ggplot2::geom_bar(stat = "identity", fill = "darkblue") +
    ggplot2::labs(title = "Nombre d'elus par code professionnel",
                  x = "Nombre d'elus",
                  y = "Code professionnel") +
    ggplot2::theme_minimal()
}


# --------------- Fonction 5 : summary.commune ---------------
#' Resume des donnees d'une commune
#'
#' Cette fonction genere un resume pour les objets de classe `commune`.
#' Elle affiche le nom de la commune, le nombre total d'elus et la repartition professionnelle.
#'
#' @param obj Un objet de classe `commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return Un resume des informations de la commune sous forme de liste.
#' @import magrittr
#' @export
summary_commune <- function(obj, ...) {
  # Verification de la classe
  if (!inherits(obj, "commune")) {
    stop("L'objet n'est pas de classe 'commune'")
  }

  # Creation du resume
  result <- list(
    nom_commune = unique(obj$Libelle.de.la.commune),
    nombre_elus = nrow(obj),
    repartition_professionnelle = obj %>%
      dplyr::count(Code.de.la.categorie.socio.professionnelle, name = "n")
  )

  # Attribution de la classe S3 pour l'affichage
  class(result) <- "summary.commune"

  return(result)
}

#' Methode S3 pour summary.commune
#'
#' Cette fonction redirige automatiquement `summary()` vers `summary_commune()`
#' pour les objets de classe `commune`.
#'
#' @param object Un objet de classe `commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @export
summary.commune <- function(object, ...) {
  summary_commune(object, ...)
}

#' Affichage du resume pour summary.commune
#'
#' Cette fonction definit l'affichage personnalise pour les objets de classe `summary.commune`.
#'
#' @param x Un objet de classe `summary.commune`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return NULL (resume affiche dans la console)
#' @export
print.summary.commune <- function(x, ...) {
  cat("Resume de la commune :\n")
  cat("Nom de la commune :", x$nom_commune, "\n")
  cat("Nombre total d'elus :", x$nombre_elus, "\n")
  cat("\nRepartition professionnelle :\n")
  print(x$repartition_professionnelle)
}


# --------------- Fonction 6 : summary.departement ---------------
#' Summary des Departements
#'
#' Cette fonction genere un resume pour les objets de classe `departement`.
#' Elle affiche le nom du departement, le nombre total de communes, et le nombre total d'elus.
#'
#' @param object Un objet de classe `departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return Un resume des informations du departement sous forme de liste.
#' @export
summary_departement <- function(object, ...) {

  # Verification de la classe de l'objet
  if (!inherits(object, "departement")) {
    stop("L'objet n'est pas de classe 'departement'")
  }

  # Calcul du nom du departement
  nom_departement <- unique(object$Libelle.du.departement)

  # Calcul du nombre total de communes
  nb_communes <- object |>
    dplyr::distinct(Code.de.la.commune) |>
    nrow()

  # Calcul du nombre total d'elus
  nb_elus <- nrow(object)

  # Creation du resume sous forme de liste
  result <- list(
    nom_departement = nom_departement,
    nombre_communes = nb_communes,
    nombre_elus = nb_elus
  )

  # Attribution de la classe "summary.departement" a la liste
  class(result) <- "summary.departement"

  return(result)
}

#' Methode S3 pour summary.departement
#'
#' Cette fonction redirige automatiquement `summary()` vers `summary_departement()`
#' pour les objets de classe `departement`.
#'
#' @param object Un objet de classe `departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @export
summary.departement <- function(object, ...) {
  summary_departement(object, ...)
}

#' Affichage du resume pour summary.departement
#'
#' Cette fonction definit l'affichage personnalise pour les objets de classe `summary.departement`.
#'
#' @param x Un objet de classe `summary.departement`.
#' @param ... Arguments supplementaires (non utilises).
#'
#' @return NULL (resume affiche dans la console)
#' @export
print.summary.departement <- function(x, ...) {
  cat("Resume du departement :\n")
  cat("Nom du departement :", x$nom_departement, "\n")
  cat("Nombre total de communes :", x$nombre_communes, "\n")
  cat("Nombre total d'elus :", x$nombre_elus, "\n")
}







# --------------- Fonction 9 : creer_commune ---------------
#' Cette fonction transforme un dataframe representant une seule commune en un objet de classe "commune".
#'
#' @description
#' La fonction s'assure que le dataframe contient une seule commune distincte,
#' et lui attribue la classe `"commune"` s'il ne l'a pas deja.
#'
#' @param df Un dataframe contenant au moins la colonne suivante :
#'   - `Code.de.la.commune` (character ou numeric) : Le code identifiant la commune.
#'
#' @return Un dataframe de classe `"commune"`, pret a etre utilise dans d'autres fonctions specifiques aux communes.
#'
#' @details
#' - La fonction verifie que la colonne `Code.de.la.commune` est presente dans `df`.
#' - Elle s'assure qu'il n'y a **qu'une seule commune distincte**, sinon elle genere une erreur.
#' - Si la classe `"commune"` n'est pas deja presente, elle est ajoutee a l'objet.
#'
creer_commune <- function(df) {
  # Verifier si la colonne 'Code.de.la.commune' existe dans le dataframe
  if (!"Code.de.la.commune" %in% colnames(df)) {
    stop("Le dataframe doit contenir la colonne 'Code.de.la.commune'.")
  }

  # Verifier qu'il y a une seule commune unique
  unique_communes <- unique(df$Code.de.la.commune)
  if (length(unique_communes) > 1) {
    stop("Le dataframe contient plusieurs communes. Fournissez les donnees d'une seule commune.")
  }

  # Ajouter la classe "commune" si elle n'est pas deja presente
  if (!inherits(df, "commune")) {
    class(df) <- c("commune", class(df))
  }

  return(df)
}


# --------------- Fonction 10 : creer_departement ---------------
#' Creer un Departement
#'
#' Cette fonction transforme un dataframe representant un departement en un objet de classe "departement".
#'
#' @param df Un dataframe contenant au moins la colonne `Code.du.departement`.
#'
#' @return Un dataframe de classe `"departement"`, qui peut etre utilise avec des fonctions specifiques aux departements.
#'
#' @details
#' - La fonction verifie que la colonne `Code.du.departement` est presente dans `df`.
#' - Elle s'assure que le dataframe represente un seul departement, sinon elle genere une erreur.
#' - La classe `"departement"` est ajoutee a l'objet pour permettre un traitement specifique.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Code.du.departement = rep("75", 10),
#'   Nom.du.departement = rep("Paris", 10),
#'   Population = c(100, 200, 150, 180, 220, 250, 300, 270, 260, 280)
#' )
#' departement <- creer_departement(data)
#' class(departement)  # Verifier la classe
#' }
#'
#' @export
creer_departement <- function(df) {
  # Verifier la presence de la colonne cle
  if (!"Code.du.departement" %in% colnames(df)) {
    stop("Le dataframe doit contenir la colonne 'Code.du.departement'.")
  }

  # Verifier s'il y a plusieurs departements
  unique_departements <- unique(df$Code.du.departement)
  if (length(unique_departements) > 1) {
    stop("Le dataframe contient plusieurs departements. Fournissez les donnees d'un seul departement.")
  }

  # Ajouter la classe "departement"
  if (!inherits(df, "departement")) {
    class(df) <- c("departement", class(df))
  }

  return(df)
}



