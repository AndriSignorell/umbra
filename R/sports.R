
#' Sporteinheiten und persönliche Merkmale
#'
#' Liefert Daten zur wöchentlichen Anzahl Sporteinheiten in Abhängigkeit von
#' Bildung, Wohnort und Alter.
#'
#' @return ein gelabelter `data.frame` mit persönlichen Merkmalen und Anzahl
#'   Sporteinheiten.
#'
#' @details Geeignete Verfahren sind empirische Verteilungen, bivariate
#' Analysen sowie einfache und multiple Regression.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.10 Diskrete Verteilungen
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
sports <- function() {
  
  d.set <- .packageData("sports.xlsx")
  
  bedrock::label(d.set, TRUE) <- c(
    "Anzahl Schuljahre",
    "Wohnhaft in einer Stadt (1=ja, 0=nein)",
    "Alter der Person",
    "Anzahl Sport-Einheiten pro Woche"
  )
  
  
  bedrock::label(d.set,) <- "
    In einer Studie zum Thema 'Sport und Gesundheit' interessierte man 
    sich dafür, welche persönlichen Eigenschaften die Anzahl absolvierter 
    Sporteinheiten beeinflussen.<br>
    Der Datensatz &nbsp;&nbsp;<strong>&link&</strong>&nbsp; 
    enthält die Angaben."
  
  return(d.set)
  
  
}


