
#' Dosiergenauigkeit eines Inhalators
#'
#' Erzeugt Prüfdaten zur abgegebenen Wirkstoffdosis in Abhängigkeit von
#' Wartezeit, Ausrichtung und Reinigungsstoss.
#'
#' @return ein gelabelter `data.frame` mit den Prüfbedingungen und der
#'   abgegebenen Dosis.
#'
#' @details Geeignete Verfahren sind Gruppenvergleiche, Varianzanalyse sowie
#' einfache und multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.16 Varianzanalyse
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
dose <- function(){
  
  d.set <- data.frame(proben_id=sample(1000:9999, n <- 507),
                      wartezeit = roundTo(abs(rnorm(n, 12, 4))), 
                      lage= relevel(factor(sample(c("aufrecht","kopfüber"), n, replace = TRUE,
                                                  prob = c(0.5,.5))), ref = "aufrecht"),
                      reinigung= relevel(factor(sample(c("ja","nein"), n, replace = TRUE,
                                                       prob = c(0.5,.5))), ref = "ja"),
                      dosis = 1)
  
  d.set$dosis <- abs(round(10 + 4*d.set$wartezeit + (d.set$lage=="aufrecht")*500 +
                             + (d.set$reinigung == "ja") * 0.8
                           + rnorm(nrow(d.set), mean=0, sd=500), -1))/2000
  bedrock::label(d.set, TRUE) <- c("ID der Probe","Wartezeit in [s] zwischen 2 Inhalationen",
                                   "Ausrichtung der Sprühdose",
                                   "Reinigungsstoss vor der Benutzung", "Abgegebene Dosis in [mg/l]")
  
  bedrock::label(d.set,) <- 'Eine Sprühdose (sog. Inhalator) mit dem Wirkstoff "Fenoterol", 
                   ein Medikament für Asthmatiker, sollte bei Betätigung des Dosierventils
                   möglichst gleichmässige Dosen abgeben. 
                   Die Stellung der Dose und der zeitliche Abstand zwischen zwei Inhalationen 
                   dürfen aus Sicherheitsgründen keine Rolle spielen. In einer Kontrolle 
                   wurde eine Stichprobe solcher Inhalatoren von einem Analyseinstitut daraufhin 
                   geprüft, ob die abgegebene Dosis diesen Anforderungen entsprach.'
  
  return(d.set)
  
}


