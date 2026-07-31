
#' Wachstum von Sonnenblumen
#'
#' Erzeugt Daten zur Pflanzenhöhe nach Dünger, Sorte und
#' Sonneneinstrahlung.
#'
#' @param n anzahl der zu erzeugenden Pflanzen; für ein balanciertes
#'   Düngerdesign sollte `n` gerade sein.
#'
#' @return ein gelabelter `data.frame` mit Dünger, Sorte,
#'   Sonneneinstrahlung und Pflanzenhöhe.
#'
#' @details Geeignete Verfahren sind Gruppenvergleiche, mehrfaktorielle
#' Varianzanalyse und multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.16 Varianzanalyse
#' @concept 1.19 Multiple lineare Regression
#' @export
sonnenblumen <- function(n=60) {
  
  dünger <- rep(c("A", "B"), each = n/2)
  sorte <- rep(c("TeddyBear", "MongolianGiant", "VelvetQueen"), length.out = n)
  sonne <- sample(c("hoch", "mittel", "gering"), size = n, replace = TRUE)
  
  # Baseline für Sorten (in cm)
  sorten_effekt <- c(TeddyBear = 150, MongolianGiant = 180, VelvetQueen = 120)
  
  # Sonneneinstrahlungseffekt
  sonnen_effekt <- c(hoch = 20, mittel = 10, gering = 0)
  
  # Generierung der Pflanzenhöhe
  höhe <- sorten_effekt[sorte] +
    sonnen_effekt[sonne] +
    ifelse(dünger == "A", 5, 0) +     # Dünger A leicht besser
    rnorm(n, mean = 0, sd = 8)        # zufällige Streuung
  
  # Zusammenführen
  daten <- data.frame(dünger, sorte, sonne, höhe)
  
  bedrock::label(daten, TRUE) <- c("Düngertyp", 
                                   "Sonnenblumensorte",
                                   "Intensität der Sonneneinstrahlung",
                                   "gemessene Pflanzenhöhe nach definierter Wachstumsperiode")
  
  bedrock::label(daten) <- "Führt die Verwendung von Dünger A im Vergleich zu Dünger B 
    zu einem höheren Wachstum (Pflanzenhöhe) bei Sonnenblumen, unter 
    Berücksichtigung verschiedener Sorten und unterschiedlicher 
    Sonneneinstrahlung?"
  
  return(daten)
  
}


