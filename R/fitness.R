

#' Leistungszuwachs durch Fitnesstraining
#'
#' Erzeugt Daten zum Leistungszuwachs nach drei Trainingsmethoden unter
#' Berücksichtigung von Geschlecht, Alter und Körpergewicht.
#'
#' @param n anzahl der zu erzeugenden Teilnehmenden.
#'
#' @return ein gelabelter `data.frame` mit Personenmerkmalen, Trainingsmethode
#'   und Leistungszuwachs.
#'
#' @details Geeignete Verfahren sind t-Tests, Varianzanalyse und multiple
#' lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.14 t-Tests
#' @concept 1.16 Varianzanalyse
#' @concept 1.19 Multiple lineare Regression
#' @export
fitness <- function(n){
  
  d.set <- within(
    data.frame(
      geschlecht  = factor(sample(c("m", "w"), 
                                  size = n, replace = TRUE)), 
      alter     = round(runif(n, min = 18, max = 40), 0),  
      training  = factor(sample(c("HYP", "INT","FUN"), 
                                size = n, replace = TRUE)) 
    ),
    zuwachs <- 
      round(( - 0.05 * alter  - 4 * (geschlecht=="w") + 
                30 * N(training)/3 + rnorm(n, mean = 0, sd = 5)), 
            0)
  )
  d.set$kgewicht <- rnorm(nrow(d.set), mean = 70, sd=6.2)
  d.set$kgewicht[d.set$geschlecht == "w"] <- rnorm(length(d.set$kgewicht[d.set$geschlecht == "w"]), 
                                                   mean=65, sd=4.2)
  d.set$kgewicht <- round(d.set$kgewicht, 1)
  
  bedrock::label(d.set, TRUE) <- c("Geschlecht",     
                                   "Alter",
                                   "Trainingtyp", 
                                   "Leistungszuwachs [in kg]",
                                   "Körpergewicht [in kg]"
  )
  
  bedrock::label(d.set,) <- 'In einem achtwöchigen Experiment soll untersucht werden, 
                  welche von drei Trainingsmethoden den grössten Leistungszuwachs 
                  im Fitnessstudio bewirkt. Dazu nehmen &n& gesunde, untrainierte 
                  Erwachsene im Alter von 18 bis 40 Jahren teil, die per Zufall auf 
                  drei Gruppen aufgeteilt werden. Die erste Gruppe 
                  absolviert klassisches "Hypertrophietraining" (<em>HYP</em>). 
                  Die zweite Gruppe trainiert nach dem "High-Intensity-Training-Prinzip" 
                  (<em>INT</em>) mit einem Satz bis zur Muskelerschöpfung. 
                  Die dritte Gruppe führt "Functional Training" mit instabilen 
                  Übungen wie TRX oder Bosu Ball durch (<em>FUN</em>). Alle Teilnehmenden 
                  trainieren dreimal pro Woche unter Aufsicht von Fitnesstrainern, 
                  um eine korrekte Übungsausführung sicherzustellen. Der 
                  Leistungszuwachs (<em>zuwachs</em>) wird primär anhand der Maximalkraft 
                  (Mittelwert aus Bankdrücken und Beinpresse) gemessen. 
                  Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;
                  enthält die Ergebnisse.'
  
  # Sekundäre Endpunkte sind Veränderungen im Muskelumfang 
  # (Oberarm/Oberschenkel).
  
  return(d.set)
  
}


