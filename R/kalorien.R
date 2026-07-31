
#' Kalorienaufnahme nach Ernährungsform
#'
#' Liefert Daten zur täglichen Kalorienaufnahme von Personen mit und ohne
#' fleischlose Ernährung.
#'
#' @return ein gelabelter `data.frame` mit Studiengruppe und Kalorienaufnahme.
#'
#' @details Geeignete Verfahren sind Kennzahlen, Konfidenzintervalle,
#' Zweistichproben-t-Test und nichtparametrische Zweistichprobentests.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.12 Stichproben und Konfidenzintervalle
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.14 t-Tests
#' @concept 1.17 Nichtparametrische Tests
#' @export
kalorien <- function() {
  
  # kalorien()
  
  d.set <- .packageData("kalorien.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Studien Gruppe", "Anzahl aufgenommene Kalorien")
  
  bedrock::label(d.set,) <-  "In einer Studie soll untersucht werden, ob Personen mit fleischloser Ernährung 
                    am Tag weniger Kalorien zu sich nehmen als Personen, die Fleisch konsumieren. 
                    Dabei wird angenommen, dass die tägliche Kalorienmenge normalverteilt ist und dass 
                    die Varianz bei beiden Gruppen übereinstimmt."
  
  return(d.set)
  
}

