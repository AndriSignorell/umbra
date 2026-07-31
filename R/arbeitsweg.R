
#' Dauer des Arbeitswegs
#'
#' Liefert Mobilitätsdaten zur Dauer des Arbeitswegs nach Agglomeration und
#' Verkehrsmittel.
#'
#' @return ein gelabelter `data.frame` mit Person, Agglomeration,
#'   Verkehrsmittel und Wegdauer.
#'
#' @details Geeignete Verfahren sind Gruppenvergleiche, Varianzanalyse und
#' nichtparametrische Mehrstichprobentests.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.16 Varianzanalyse
#' @concept 1.17 Nichtparametrische Tests
#' @export
arbeitsweg <- function(){
  
  d.set <- .packageData("arbeitsweg.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("ID der befragten Person", "Agglomeration (<em>ZH, BE, BS</em>)", 
                                   "Transportmittel", "Benötigte Zeit für den Arbeitsweg")
  
  bedrock::label(d.set,) <- "
          Für eine Mobilitätsuntersuchung zuhanden der Verkehrskommission sollte die Dauer
          des Arbeitswegs für unterschiedliche Agglomerationen und Verkehrsmittel ermittelt werden.
          "
  
  return(d.set)
  
}


