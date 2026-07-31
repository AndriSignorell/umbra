
#' Zahlungsdauer von Krankenversicherern
#'
#' Liefert Kennzahlen Schweizer Krankenversicherer zu Grösse, Marktanteil und
#' mittlerer Zahlungsdauer.
#'
#' @return ein gelabelter `data.frame` mit Versicherungs- und
#' Zahlungsmerkmalen.
#'
#' @details Geeignete Verfahren sind Kennzahlen, bivariate Analysen und
#' einfache lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.18 Einfache lineare Regression
#' @export
krankenversicherer <- function(){
  
  d.set <- .packageData("krankenversicherer.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Name der Versicherers",      # Versicherer
                                   "CH Marktanteil in [Prozent]",      # Marktanteil
                                   "Grössenklasse",              # Typ
                                   "Anzahl Versicherte",         # Versicherte
                                   "Anzahl versicherte Kinder",  # Kinder
                                   "Anzahl versicherte Junge Erwachsene",# Junge Erwachsene 
                                   "Anzahl versicherte Erwachsene",# Erwachsene 
                                   "Mittlere Zahlungsdauer [in Tagen]" # ZahlDauer
  )
  
  bedrock::label(d.set,) <- "
              Für die Kunden der Krankenkassen kann es ein wichtiges 
              Merkmal sein, wie schnell dass eingereichte Rechnungen vergütet 
              werden. Für das Jahr 2014 veröffentlichte das Bundesamt für Gesundheit 
              eine entsprechende Studie, bei der pro Versicherer die mittlere Anzahl 
              Tage ausgewiesen wurde, die zwischen dem Einreichen der Rechnung und 
              der Auszahlung lag. Vermutet wird, dass die Auszahlungsgeschwindigkeit von
              der Grösse des Versicherers abhängen könnte.<br>
              "
  
  
  return(d.set)
  
}

