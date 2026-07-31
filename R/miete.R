
#' Mietspiegeldaten
#'
#' Liefert einen Datensatz zu Wohnungsmieten sowie Ausstattungs- und
#' Lagemerkmalen.
#'
#' @return ein gelabelter `data.frame` mit Miet- und Wohnungsmerkmalen.
#'
#' @details Geeignete Verfahren sind empirische Verteilungen, Kennzahlen,
#' bivariate Analysen und multiple lineare Regression.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.19 Multiple lineare Regression
#' @export
miete <- function() {
  
  # miete()
  
  d.set <- .packageData("miete.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Monatsmiete in [EUR]", "Fläche der Wohnung in [m<sup>2</sup>]", 
                                   "Bad frisch renoviert (j/n)", "Zentralheizung (j/n)","Ausbaustandard der Küche",
                                   "Mietvertragsdauer", "Baujahr kategorisiert", "Wohngegend")
  
  bedrock::label(d.set,) <- "Dieser Datensatz enthält einen repräsentativen Auszug 
                   aus Daten, die anlässlich der Erstellung 
                   eines Mietspiegels in einer grösseren europäischen Stadt erhoben wurden. Ziel 
                   eines Mietspiegels ist die Bestimmung der sogenannten ortsüblichen Miete, deren 
                   Höhe in der Regel von Ausstattungs- und Lagemerkmalen der Mietwohnung 
                   abhängt."
  
  
  return(d.set)
  
}



