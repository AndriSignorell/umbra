
#' Kredithöhe und Kundenmerkmale
#'
#' Erzeugt Bankkundendaten zur Kredithöhe in Abhängigkeit von Finanzwissen,
#' Einkommen und Beschäftigungsstatus.
#'
#' @param n anzahl der zu erzeugenden Kunden.
#'
#' @return ein gelabelter `data.frame` mit Kundenmerkmalen und Kredithöhe.
#'
#' @details Geeignete Verfahren sind bivariate Analysen sowie einfache und
#' multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
kredit <- function(n){
  
  d.dat <- within(
    data.frame(
      finanzwissen   = round(runif(n, min = 1, max = 10), 0), 
      einkommen      = round(runif(n, min = 40, max = 150), 0),  
      beschäftigung  = factor(sample(c("teilzeit", "vollzeit"), 
                                     size = n, replace = TRUE)) 
    ),
    kredit <- 
      round(10 + 5 * finanzwissen + 0.8 * einkommen + 
              15 * N(beschäftigung) + rnorm(n, mean = 0, sd = 5), 
            0)
  )
  
  label(d.dat, TRUE) <- c("Finanzwissen [numerisch, auf einer Skala von 1 bis 10]",
                          "Jahreseinkommen [in Tausend CHF]",
                          "Beschäftigungsstatus",
                          "Kredithöhe")
  
  bedrock::label(d.dat) <- gettextf("
  Eine Bank möchte untersuchen, welche Faktoren die Höhe eines gewährten Kredits 
  beeinflussen. Dazu wurden Daten von %s Kunden erfasst. 
  Die Zielvariable y ist die Höhe des gewährten Kredits (in Tausend CHF). <br>
  Folgende erklärende Variablen wurden im Datensatz&nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;
  erfasst: <br><br> &vartab&", n)
  
  # Führen Sie eine multiple lineare Regression durch, um den Einfluss dieser Variablen 
  # auf die schulischen Leistungen zu analysieren.
  
  return(d.dat)
  
}


