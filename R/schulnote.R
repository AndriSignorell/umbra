
#' Schulnoten und Lebensgewohnheiten
#'
#' Erzeugt Daten zur schulischen Leistung in Abhängigkeit von Bildschirmzeit,
#' Schlafdauer und Freizeitaktivitäten.
#'
#' @return ein gelabelter `data.frame` mit den Einflussgrössen und der Note.
#'
#' @details Geeignete Verfahren sind bivariate Analysen und multiple lineare
#' Regression.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.19 Multiple lineare Regression
#' @export
schulnote <- function(){
  
  d.dat <- within(
    data.frame(
      bildschirmzeit      = round(runif(n, min = 2, max = 8), 2),
      schlafdauer         = round(runif(n, min = 5, max = 9), 2),
      freizeitaktivitäten = ordered(sample(1:3, size = n, replace = TRUE),
                                    labels=c("selten", "gelegentlich", "häufig"))
    ),
    note <- Winsorize(
      round(2 + -0.3*bildschirmzeit + 0.4*schlafdauer + 
              0.2* N(freizeitaktivitäten) + rnorm(n, mean = 0, sd = 0.5), 
            2), val = c(1,6))
  )
  
  label(d.dat, TRUE) <- c("Bildschirmzeit [Stunden pro Tag]",
                          "Schlafdauer [Stunden pro Nacht]",
                          "Häufigkeit von Freizeitaktivitäten","Schulische Leistung")
  
  
  bedrock::label(d.dat) <- "
    Ein Sozialwissenschaftler möchte untersuchen, wie verschiedene Faktoren die 
    schulischen Leistungen von Jugendlichen beeinflussen.
    Als Zielvariable Y dienen die schulischen Leistungen, 
    gemessen als Durchschnittsnote auf einer Skala von 1 (sehr schwach) bis 6 (sehr gut).
    Erklärende Variablen sind:<br><br>&vartab&<br><br>
      
    Es wurde eine Stichprobe von &n& Jugendlichen untersucht
    (Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;). 
    "
  
  # Führen Sie eine multiple lineare Regression durch, um den Einfluss dieser Variablen 
  # auf die schulischen Leistungen zu analysieren.
  
  return(d.dat)
  
}


