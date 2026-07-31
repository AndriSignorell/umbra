
#' Einflussfaktoren des Zigarettenkonsums
#'
#' Liefert Daten zu Zigarettenkonsum, Preisen, Rauchverboten und
#' soziodemografischen Merkmalen.
#'
#' @param vars namen der Variablen, die zurückgegeben werden sollen.
#'
#' @return ein gelabelter `data.frame` mit den ausgewählten Variablen.
#'
#' @details Geeignete Verfahren sind Kennzahlen, bivariate Analysen,
#' Signifikanztests sowie einfache und multiple Regression.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
smoke <- function(vars=c("educ","cigpric","white","age","income","cigs","restaurn","cigcat")) {
  
  d.set <- .packageData("smoke.xlsx")
  
  d.set$pers_id <- sample(1000:9999, nrow(d.set))
  
  bedrock::label(d.set, TRUE) <- c("Anzahl Schuljahre",
                                   "Durchschnittlicher Preis pro Packung",
                                   "Hautfarbe weiss (1=ja, 0=nein)",
                                   "Alter der Person",
                                   "Einkommen in [$/Jahr]",
                                   "Anzahl gerauchte Zigaretten pro Tag",
                                   "Gilt ein Rauchverbot in Restaurants im Staat (1=ja, 0=nein)",
                                   "Anzahl gerauchte Zigaretten pro Tag (kategorisiert)",
                                   "ID der Person")
  
  d.set <- d.set[, vars]
  
  bedrock::label(d.set,) <- "
        Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;enthält Daten zu einer Raucher-Studie, die in 
        den Vereinigten Staaten durchgeführt wurde. In dieser Studie interessierte man sich dafür, 
        welche Effekte die Anzahl pro Tag gerauchter Zigaretten beeinflussen. "
  
  return(d.set)
  
}


