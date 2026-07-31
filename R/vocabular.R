
#' Ergebnisse eines Wörtertests
#'
#' Liefert Prüfungsergebnisse aus zwei Jahren zusammen mit Geschlecht und
#' Ausbildungsdauer der Prüflinge.
#'
#' @return ein gelabelter `data.frame` mit Prüfungsjahr, Geschlecht,
#'   Ausbildungsdauer und Testergebnis.
#'
#' @details Geeignete Verfahren sind Gruppenvergleiche, Varianzanalyse sowie
#' einfache und multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.14 t-Tests
#' @concept 1.16 Varianzanalyse
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
vocabular <- function(){
  
  d.set <- .packageData("vocabular.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Jahr der Prüfung",     
                                   "Geschlecht des Prüflings",
                                   "Anzahl Schul-/Ausbildungsjahre", 
                                   "Ergebnis im Wörtertest"
  )
  
  bedrock::label(d.set,) <- "Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;
                  enthält die Ergebnisse eines Wörter-Tests aus zwei verschiedenen Jahren.
                  Die Lehrer fragen sich, ob es Faktoren bei den Prüflingen gibt, die die 
                  Ergebnisse erklären können. Sie vermuten (oder hoffen zumindest), dass 
                  die Ausbildungszeit einen (positiven) Einfluss hat. In der Frage, ob das 
                  Geschlecht relevant ist, sind sie sich uneins.
                  Zudem wurden in den beiden betrachteten Jahren unterschiedliche 
                  didaktische Konzepte verfolgt, sodass auch dies einen Einfluss auf 
                  die Prüfungsergebnisse gehabt haben könnte."
  
  return(d.set)
  
}


