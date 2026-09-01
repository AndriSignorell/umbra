
#' Umsätze Schweizer Detailhändler
#'
#' Liefert eine Rangliste von Schweizer Detailhandelsunternehmen und deren
#' geschätzten Jahresumsätzen.
#'
#' @return ein gelabelter `data.frame` mit Unternehmen und Jahresumsatz.
#'
#' @details Geeignete Verfahren sind empirische Verteilungen, Kennzahlen,
#' Lorenzkurve und Konzentrationsmasse.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.6 Konzentration
#' @export
detailhandel <- function(){
  
  d.set <- .packageData("detailhandel.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Identifikationsnummer des Unternehmens", 
                                   "Name des Unternehmens",
                                   "gesch\u00e4tzter Jahresumsatz in [Mio CHF]"
  )
  
  bedrock::label(d.set,) <- "
              Die Liste enth\u00e4lt den Jahresumsatz von &n& auf dem Schweizer Markt 
              aktiven Unternehmen aus dem Detailhandel.<br>
              "
  
  return(d.set)
  
}


