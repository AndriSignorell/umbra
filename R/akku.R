
#' Lebensdauer zweier Akkutypen
#'
#' Liefert Stichprobendaten zur Anzahl Ladezyklen bis zum Ersatz für zwei
#' Akkutypen.
#'
#' @return ein gelabelter `data.frame` mit Akkutyp und Anzahl Ladezyklen.
#'
#' @details Geeignete Verfahren sind Kennzahlen, Konfidenzintervalle,
#' Zweistichproben-t-Test und nichtparametrische Zweistichprobentests.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.12 Stichproben und Konfidenzintervalle
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.14 t-Tests
#' @concept 1.17 Nichtparametrische Tests
#' @export
akku <- function(){
  
  d.set <- .packageData("akku.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Typ des Akkus",     
                                   "Anzahl Ladezyklen bis zum Ersatz" 
  )
  
  bedrock::label(d.set,) <- "Ein Batterie-Hersteller bietet 2 Typen von 
    Akkus A und B an und verspricht in der Werbung, dass beide Akkutypen 
    gleich viele Ladezyklen vertragen, bevor sie ersetzt werden müssen.
    Eine Konsumentenorganisation will das überprüfen und bildet eine Stichprobe
    mit Typ-A und Typ-B Akkus. Die Akkus werden soviele Male geladen, bis
    die Kapazität nicht mehr über einen vorher bestimmten Schwellwert kommt."
  
  return(d.set)
  
}



