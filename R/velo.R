#' Filtrer les anomalies dans les données vélo
#'
#' Supprime les lignes contenant des anomalies et les valeurs aberrantes.
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @return Un data.frame nettoyé.
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang .data
filtre_anomalie <- function(trajet) {
  trajet |>
    dplyr::filter(
      is.na(.data[["Probabilit\u00e9 de pr\u00e9sence d'anomalies"]]),
      .data[["Total"]] < 10000,
      .data[["Total"]] > 0
    )
}

#' Compter le nombre total de trajets
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @return Un nombre représentant le total des trajets.
#' @export
#' @importFrom dplyr pull
compter_nombre_trajets <- function(trajet) {
  trajet |>
    dplyr::pull("Total") |>
    sum()
}

#' Compter le nombre de boucles distinctes
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @return Un entier représentant le nombre de boucles.
#' @export
#' @importFrom dplyr pull n_distinct
compter_nombre_boucle <- function(trajet) {
  trajet |>
    dplyr::pull("Num\u00e9ro de boucle") |>
    dplyr::n_distinct()
}

#' Trouver le trajet avec le maximum de passages
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @return Un data.frame avec les infos du trajet max.
#' @export
#' @importFrom dplyr filter select slice_max pull
#' @importFrom rlang .data
trouver_trajet_max <- function(trajet) {
  trajet_max <- trajet |>
    filtre_anomalie() |>
    dplyr::slice_max(.data[["Total"]]) |>
    dplyr::select("Boucle de comptage", "Jour", "Total")

  trajet_max$moyenne_jour_identique <- trajet |>
    dplyr::filter(.data[["Jour"]] == trajet_max$Jour) |>
    dplyr::pull("Total") |>
    mean()

  trajet_max$moyenne_boucle_identique <- trajet |>
    dplyr::filter(.data[["Boucle de comptage"]] == trajet_max[["Boucle de comptage"]]) |>
    dplyr::pull("Total") |>
    mean()

  return(trajet_max)
}

#' Calculer la distribution des trajets par jour de la semaine
#'
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @param filtre Booléen, TRUE par défaut.
#' @return Un data.frame avec la distribution.
#' @export
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
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @param filtre Booléen, TRUE par défaut.
#' @return Un objet ggplot.
#' @export
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
#' @param trajet Un data.frame respectant le schéma de df_velo.
#' @param boucle Un vecteur de numéros de boucle. Si NULL, pas de filtre.
#' @return Un data.frame filtré.
#' @export
#' @examples
#' filtrer_trajet(trajet = df_velo, boucle = c("880", "881"))
#' @importFrom dplyr filter
#' @importFrom rlang .data
filtrer_trajet <- function(trajet, boucle) {
  if (is.null(boucle)) {
    return(trajet)
  }
  trajet |>
    dplyr::filter(.data[["Num\u00e9ro de boucle"]] %in% boucle)
}
