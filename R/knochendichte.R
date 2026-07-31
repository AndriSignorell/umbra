
#' Knochendichte nach Vitamindosis
#'
#' Liefert Studiendaten zur Knochendichte nach drei Vitamindosis-Gruppen.
#'
#' @return ein gelabelter `data.frame` mit Knochendichte und Dosisgruppe.
#'
#' @details Geeignete Verfahren sind Kennzahlen nach Gruppen, einfaktorielle
#' Varianzanalyse und nichtparametrische Mehrstichprobentests.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.16 Varianzanalyse
#' @concept 1.17 Nichtparametrische Tests
#' @export
knochendichte <- function() {
  
  d.set <- .packageData("knochendichte.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Knochendichte [g/cm<sup>3</sup>]",
                                   "Vitamin Dosis Gruppe"
  )
  
  bedrock::label(d.set,) <- "
      Vitamin C trägt zu einer normalen Kollagenbildung für eine normale Funktion der 
      Blutgefässe bei.
      Bei Insassen in Pflegeheimen ist die Versorgung besonders wichtig für den 
      Knochenaufbau. In einer Studie wurde eine Gruppe von
      Insassen zufällig je einer von drei Behandlungen zugeteilt, die über 6&nbsp;Wochen unterschiedliche 
      Dosen Vitamin C erhielten. Die Knochendichte in [g/cm<sup>3</sup>] wurde dann abschliessend bestimmt.
      Ein hoher Wert gilt als erstrebenswert."
  
  return(d.set)
  
  
}

