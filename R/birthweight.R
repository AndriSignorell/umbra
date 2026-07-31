
#' Geburtsgewicht und Rauchen in der Schwangerschaft
#'
#' Liefert Daten zum Geburtsgewicht sowie zu familiären und
#' soziodemografischen Einflussgrössen.
#'
#' @return ein gelabelter `data.frame` mit Geburtsgewicht und Einflussgrössen.
#'
#' @details Geeignete Verfahren sind bivariate Analysen, Gruppenvergleiche
#' sowie einfache und multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.14 t-Tests
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
birthweight <- function() {
  
  d.set <- .packageData("birthweight.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Personen-ID",
                                   "Familieneinkommen [1'000 $]",
                                   "Geburtsgewicht [kg]",
                                   "Schuljahre Vater [Jahre]",
                                   "Schuljahre Mutter [Jahre]",
                                   "Geschlecht des Kindes (0=weiblich, 1=männlich)",
                                   "Hautfarbe weiss (0=nein, 1=ja)",
                                   "Anzahl während der Schwangerschaft pro Tag <br>von der Mutter gerauchter Zigaretten" 
  )
  
  bedrock::label(d.set,) <- "Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;enthält Daten zu einer Raucher-Studie, die in 
        den Vereinigten Staaten durchgeführt wurde. In dieser Studie interessierte man unter anderem 
        sich dafür, welchen Einfluss die Anzahl pro Tag gerauchter Zigaretten von Schwangeren auf das 
        Geburtsgewicht der Kinder hatten."
  
  return(d.set)
  
  
}



