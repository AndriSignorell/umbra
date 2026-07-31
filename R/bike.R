
#' Tägliche Fahrradvermietungen
#'
#' Liefert Tagesdaten eines Fahrradverleihers mit Kalender-, Saison- und
#' Wettermerkmalen.
#'
#' @return ein gelabelter `data.frame` mit Einflussgrössen und Anzahl
#'   Vermietungen.
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
bike <- function(){
  
  d.set <- .packageData("bike.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Datum","Wochentag","Monat","Ferientag","Jahreszeit",
                                   "Wetter","Temperatur in °C","gefühlte Temperatur in °C",
                                   "Luftfeuchtigkeit in %","Windgeschwindigkeit",
                                   "Anzahl Vermietungen")
  
  bedrock::label(d.set,) <- "
          Der Verleih von Fahrrädern hängt stark mit dem Wetter und den saisonalen Gegebenheiten zusammen. 
          Typischerweise beeinflussen Wetterbedingungen, Niederschlag, Wochentag, 
          Jahreszeit, Tageszeit usw. die Anzahl Vermietungen.
          <br>Der Datensatz&nbsp;&nbsp;&link&&nbsp;&nbsp;enthält die Vermietungen eines grossen 
          Velo-Verleihers für jeden Tag während 2er Jahre."
  
  return(d.set)
  
}



