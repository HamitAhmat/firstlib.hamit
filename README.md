
# firstlib.hamit <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
![Version](https://img.shields.io/badge/version-1.0.0-blue)
<!-- badges: end -->

Package R de statistiques descriptives pour les données de comptages
vélo de Nantes Métropole (vacances de Toussaint 2025).

## Installation

```r
remotes::install_github("HamitAhmat/firstlib.hamit")
```

## Utilisation

```r
library(firstlib)

# Filtrer sur des boucles
df_selection <- filtrer_trajet(df_velo, boucle = c(880, 881))

# Graphique de distribution
plot_distribution_semaine(df_selection)
```
