
#' Umsatz und Kundenzahl von Unternehmen
#'
#' Liefert Unternehmensdaten zu geschätztem Jahresumsatz und geschätzter
#' Kundenzahl.
#'
#' @return ein gelabelter `data.frame` mit Unternehmen, Umsatz und Kundenzahl.
#'
#' @details Geeignete Verfahren sind Kennzahlen, Konzentrationsmasse,
#' Korrelation und einfache lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.6 Konzentration
#' @concept 1.18 Einfache lineare Regression
comcorp <- function(){
  
  d.set <- .packageData("comcorp.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Identifikationsnummer des Unternehmens", 
                                   "Name des Unternehmens",
                                   "geschätzter Jahresumsatz in [Mio CHF]",
                                   "geschätzter Anzahl Kunden"
  )
  
  bedrock::label(d.set,) <- "
              Die Liste enthält den Jahresumsatz von &n& auf dem Schweizer Markt 
              aktiven Unternehmen aus dem Detailhandel.<br>
              "
  
  return(d.set)
  
}


