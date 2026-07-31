
#' Stundenlöhne aus Edinburgh
#'
#' Liefert Stichprobendaten zu Stundenlöhnen im Raum Edinburgh.
#'
#' @return ein gelabelter `data.frame` mit den Lohndaten.
#'
#' @details Geeignete Verfahren sind Klasseneinteilungen, empirische
#' Verteilungen, Kennzahlen, Konzentrationsmasse und lineare Regression.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.6 Konzentration
#' @concept 1.12 Stichproben und Konfidenzintervalle
#' @concept 1.18 Einfache lineare Regression
#' @export
lohn <- function() {
  
  # use for: Klasseneinteilung, LinReg
  d.set <- .packageData("lohn.xlsx")
  
  bedrock::label(d.set,) <- " Der Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;enthält
                    Lohndaten, die in einer Stichprobe im Raum Edinburgh (Schottland) erhoben wurden.
                    <br>
                    Die Variable&nbsp;<em>stdlohn</em> gibt den Stundenlohn für den jeweiligen 
                    Arbeitnehmer/jeweilige Arbeitnehmerin an."
  
  return(d.set)
  
}

# lohn()


