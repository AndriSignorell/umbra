
#' Systolischer Blutdruck
#'
#' Erzeugt Daten zum systolischen Blutdruck in Abhängigkeit von Alter,
#' Geschlecht und Body-Mass-Index.
#'
#' @return ein gelabelter `data.frame` mit Identifikation, Alter, Geschlecht,
#'   Body-Mass-Index und systolischem Blutdruck.
#'
#' @details Geeignete Verfahren sind bivariate Analysen sowie einfache und
#' multiple lineare Regression, einschliesslich Interaktionen.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
blood <- function() {
  
  d.set <- rbind(
    data.frame(age=(a <- sample(20:70, 187, replace=TRUE)),
               sex="m", 
               bmi=(b <- round(rnorm(187, 25, 5), 1)),
               systol=a * 0.5 + 115 + (b-25) * 0.3 + rnorm(length(a), mean = 0, sd=7)),
    
    data.frame(age=(a <- sample(20:70, 157, replace=TRUE)),
               sex="f",
               bmi=(b <- round(rnorm(157, 25, 5), 1)),
               systol=a * 0.7 + 110  + (b-25) * 0.2 + rnorm(length(a), mean = 0, sd=5)))
  
  d.set$systol <- round(d.set$systol, 1)
  d.set <- data.frame(id=seq(nrow(d.set)), sampleX(d.set))
  rownames(d.set) <- NULL
  
  bedrock::label(d.set,) <- "Mit zunehmendem Alter nimmt der Blutdruck im Allgemeinen zu. 
                   In einer Studie wurde für 
                   eine Stichprobe mit Probanden beider Geschlechter der 
                   systolische Blutdruck in [mmHg] gemessen."
  
  return(d.set)
  
  # Berechnen Sie eine lineare Regression.
  # Wie gross ist die mittlere Zunahme pro Lebensjahr? 
  # Ist der Blutdruck von Frauen und Männern unterschiedlich? 
  # Ist die Zunahme für Frauen und Männer unterschiedlich. 
  #   
  # 1 mmHg = 1.33322 mbar
  # v <- "A"
  
}  

