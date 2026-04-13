#' Filtrer les anomalies dans les données vélo
#'
#' Supprime les lignes contenant des anomalies (Forte ou Faible),
#' ainsi que les valeurs aberrantes (Total > 10000 ou Total <= 0).
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#'
#' @return Un data.frame nettoyé sans les anomalies.
#' @export
#'
#' @importFrom dplyr filter
filtre_anomalie <- function(trajet) {
  trajet |>
    dplyr::filter(
      is.na(`Probabilité de présence d'anomalies`),
      Total < 10000,
      Total > 0
    )
}

#' Compter le nombre total de trajets
#'
#' Calcule la somme de la colonne Total.
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#'
#' @return Un nombre représentant le total des trajets.
#' @export
#'
#' @importFrom dplyr pull
compter_nombre_trajets <- function(trajet) {
  trajet |>
    dplyr::pull(Total) |>
    sum()
}

#' Compter le nombre de boucles distinctes
#'
#' Compte le nombre de boucles de comptage uniques dans le jeu de données.
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#'
#' @return Un entier représentant le nombre de boucles distinctes.
#' @export
#'
#' @importFrom dplyr pull n_distinct
compter_nombre_boucle <- function(trajet) {
  trajet |>
    dplyr::pull(`Numéro de boucle`) |>
    dplyr::n_distinct()
}

#' Trouver le trajet avec le maximum de passages
#'
#' Identifie la paire boucle-jour ayant le plus grand nombre de passages,
#' après filtrage des anomalies. Retourne aussi la moyenne par jour
#' et la moyenne par boucle.
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#'
#' @return Un data.frame avec le nom de la boucle, le jour, le total,
#'   la moyenne par jour et la moyenne par boucle.
#' @export
#'
#' @importFrom dplyr filter select slice_max pull
trouver_trajet_max <- function(trajet) {
  trajet_max <- trajet |>
    filtre_anomalie() |>
    dplyr::slice_max(Total) |>
    dplyr::select(`Boucle de comptage`, Jour, Total)

  trajet_max$moyenne_jour_identique <- trajet |>
    dplyr::filter(Jour == trajet_max$Jour) |>
    dplyr::pull(Total) |>
    mean()

  trajet_max$moyenne_boucle_identique <- trajet |>
    dplyr::filter(`Boucle de comptage` == trajet_max$`Boucle de comptage`) |>
    dplyr::pull(Total) |>
    mean()

  return(trajet_max)
}

#' Calculer la distribution des trajets par jour de la semaine
#'
#' Compte la somme des trajets pour chaque jour de la semaine.
#' Possibilité de filtrer les anomalies avant le calcul.
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @param filtre Un booléen. Si TRUE, les anomalies sont filtrées
#'   avant le calcul. Par défaut TRUE.
#'
#' @return Un data.frame avec le jour de la semaine et le nombre de trajets.
#' @export
#'
#' @importFrom dplyr count
#' @importFrom rlang .data
calcul_distribution_semaine <- function(trajet, filtre = TRUE) {
  if (filtre) {
    trajet <- filtre_anomalie(trajet)
  }
  trajet |>
    dplyr::count(.data[["Jour de la semaine"]], wt = .data[["Total"]], sort = TRUE, name = "trajets")
}

#' Afficher la distribution des trajets par jour de la semaine
#'
#' Produit un diagramme en colonnes. Possibilité de filtrer
#' les anomalies avant le calcul.
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @param filtre Un booléen. Si TRUE, les anomalies sont filtrées
#'   avant le calcul. Par défaut TRUE.
#'
#' @return Un objet ggplot.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_col
#' @importFrom forcats fct_recode
#' @importFrom dplyr mutate
#' @importFrom rlang .data
plot_distribution_semaine <- function(trajet, filtre = TRUE) {
  trajet_weekday <- calcul_distribution_semaine(trajet, filtre = filtre) |>
    dplyr::mutate(
      jour = forcats::fct_recode(
        factor(.data[["Jour de la semaine"]]),
        "lundi" = "1",
        "mardi" = "2",
        "mercredi" = "3",
        "jeudi" = "4",
        "vendredi" = "5",
        "samedi" = "6",
        "dimanche" = "7"
      )
    )

  ggplot2::ggplot(trajet_weekday) +
    ggplot2::aes(x = .data[["jour"]], y = .data[["trajets"]]) +
    ggplot2::geom_col()
}

#' Filtrer les trajets par numéro de boucle
#'
#' Filtre un data.frame pour ne garder que les boucles sélectionnées.
#' Si boucle est NULL, retourne le data.frame complet sans filtrage.
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @param boucle Un vecteur de numéros de boucle à conserver. Si NULL,
#'   aucun filtrage n'est appliqué.
#'
#' @return Un data.frame filtré contenant uniquement les boucles demandées.
#' @export
#'
#' @examples
#' filtrer_trajet(trajet = df_velo, boucle = c("880", "881"))
#' filtrer_trajet(trajet = df_velo, boucle = NULL)
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
filtrer_trajet <- function(trajet, boucle) {
  if (is.null(boucle)) {
    return(trajet)
  }
  trajet |>
    dplyr::filter(.data[["Numéro de boucle"]] %in% boucle)
}
