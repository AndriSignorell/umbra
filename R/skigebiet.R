
#' Kennzahlen Schweizer Skigebiete
#'
#' Liefert Daten zu Pistenlänge, Besucherzahl und Jahresumsatz Schweizer
#' Skigebiete.
#'
#' @return ein gelabelter `data.frame` mit Merkmalen der Skigebiete.
#'
#' @details Geeignete Verfahren sind Kennzahlen, bivariate Analysen,
#' Konzentrationsmasse sowie einfache und multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.6 Konzentration
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
skigebiet <- function(){
  
  d.set <- .packageData("skigebiet.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Name des Skigebiets",
                                   "Pistenkilometer in [km]",
                                   "geschätzter Anzahl Besucher pro Jahr",
                                   "geschätzter Jahresumsatz in [Mio CHF]"
  )
  
  bedrock::label(d.set,) <- "
              Die Liste enthält den die Anzahl Pistenkilometer, die Anzahl 
              Besucher und den geschätzten Jahresumsatz von &n& Schweizer
              Skigebiete.<br>
              "
  
  return(d.set)
  
}


