
#' Gewichte von Äschen in zwei Jahren
#'
#' Erzeugt zwei unabhängige Stichproben von Fischgewichten für die Jahre 2023
#' und 2024.
#'
#' @param mu numerischer Vektor mit den Mittelwerten beider Jahre.
#' @param s numerischer Vektor mit den Standardabweichungen beider Jahre.
#'
#' @return ein gelabelter `data.frame` mit Jahr und Fischgewicht.
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
aeschen <- function(mu =c(1, 0.8), s=c(0.19, 0.22)){
  
  # get base dataset
  d.set <- twoSamp(
    n = c(101, 123),
    rdist = list(
      function(n) round(rnorm(n, mean = mu[1], sd = s[1]), 3),
      function(n) round(rnorm(n, mean = mu[2], sd = s[2]), 3)
    ),
    groups = 2023:2024,
    valueName = "gewicht",
    groupName = "jahr"
  )
  
  bedrock::label(d.set,) <- "Fischer im Bodensee \u00e4usserten den Verdacht, dass das Gewicht der Aeschen 
      im 2024 deutlich kleiner sei, als im vorherigen Jahr (2023). Dieses war speziell
      warm gewesen und man vermutete, dass vor allem \u00e4ltere (und schwerere) Fische besonders unter 
      dem in w\u00e4rmerem Wasser verringerten Sauerstoffgehalt gelitten h\u00e4tten und allenfalls vorzeitig 
      eingegangen sein k\u00f6nnten.<br>
      Der Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;enth\u00e4lt
      das Gewicht der Fische f\u00fcr je eine Stichprobe aus den Jahren 2023 und 2024."
  
  return(d.set)
  
}


