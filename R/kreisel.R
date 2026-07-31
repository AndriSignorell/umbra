
#' Staulänge mit und ohne Verkehrsleitung
#'
#' Liefert Stichprobendaten zur morgendlichen Staulänge an Tagen mit und ohne
#' Verkehrsleitung.
#'
#' @return ein gelabelter `data.frame` mit den Staulängen beider Bedingungen.
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
kreisel <- function(){
  
  d.set <- .packageData("kreisel.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Staulänge ohne Verkehrsleitung", 
                                   "Staulänge mit Verkehrsleitung")
  
  bedrock::label(d.set,) <- "
      Am Kreisel in Fällanden staut sich der Verkehr an jedem Morgen zuverlässig. 
      Die Gemeinde versuchte als Ansatz, die Wartezeiten für die Autofahrer 
      durch den Einsatz von Verkehrskadetten zu verkürzen. 
      Um den Einfluss der Verkehrsleitung zu prüfen, mass man an &nA& normalen Tagen 
      die Länge des Staus in [m] zu jeweils 8 vorgängig zufällig bestimmten Zeitpunkten 
      innerhalb der kritischen Zeitphase von 06:00-09:00 Uhr (A - <em>ohne</em>). 
      Das Gleiche tat man an weiteren &nB& Tagen, an denen die Verkehrskadetten 
      zum Einsatz kamen (B - <em>mit</em>). 
      Die Mittelwerte des Staus pro Tag finden sich in der 
      Datei &nbsp;<strong>&link&</strong>&nbsp;&nbsp;.
      "
  
  return(d.set)
  
}





