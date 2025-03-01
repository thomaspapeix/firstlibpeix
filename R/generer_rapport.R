#' Générer un rapport d'analyse des élus
#'
#' Cette fonction génère un rapport Quarto (`rapport.qmd`)
#' en utilisant les paramètres spécifiés pour la commune et le département.
#'
#' @param code_commune Code INSEE de la commune (ex: "75056").
#' @param code_departement Code du département (ex: "75").
#' @param output Chemin du fichier de sortie (ex: "rapport.html").
#'
#' @return Génère un fichier HTML contenant l'analyse des élus.
#' @export
generer_rapport <- function(code_commune, code_departement, output = "rapport.html") {

  # Localiser le fichier rapport.qmd dans le package
  rapport_path <- system.file("rapport.qmd", package = "firstlibpeix")


  # Vérifier si le fichier existe
  if (rapport_path == "") {
    stop("❌ Le fichier rapport.qmd est introuvable dans le package.")
  }

  # Générer le rapport avec Quarto
  quarto::quarto_render(
    input = rapport_path,
    output_file = output,
    execute_params = list(
      code_commune = code_commune,
      code_departement = code_departement
    )
  )

  message("✅ Rapport généré avec succès : ", output)
}
