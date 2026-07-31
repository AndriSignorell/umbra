
#' Einkaufsbetrag nach Filiale und Wagengrösse
#'
#' Erzeugt Einkaufsdaten für drei Filialen und zwei Einkaufswagengrössen.
#'
#' @param n anzahl der zu erzeugenden Einkäufe.
#'
#' @return ein gelabelter `data.frame` mit Filiale, Wagengrösse und
#'   Einkaufsbetrag.
#'
#' @details Geeignete Verfahren sind Gruppenvergleiche, zweifaktorielle
#' Varianzanalyse und multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.16 Varianzanalyse
#' @concept 1.19 Multiple lineare Regression
#' @export
einkauf <- function(n = 427){
  
  d.dat <- within(
    data.frame(
      filiale  = factor(sample(c("Ost", "Nordwest", "Süd"), 
                               size = n, replace = TRUE)), 
      wagen    = factor(sample(c("L", "XL"), 
                               size = n, replace = TRUE))
    ),
    
    einkauf <- 
      bedrock::roundTo((rpois(n, 
                              lambda = c(7, 9, 14)[nf(filiale)]*4 + 
                                c(10, 15)[nf(wagen)]) + runif(n)), 0.05)
  )
  
  label(d.dat, TRUE) <- c("Filiale",
                          "Einkaufswagengrösse",
                          "Einkaufsbetrag [in CHF]")
  
  bedrock::label(d.dat) <- gettextf("
  Ein Detailhändler möchte untersuchen, ob unterschiedliche Grössen der Einkaufswagen
  die Höhe des Einkaufsbetrags beeinflussen. Einer Hypothese nach könnten grosse 
  Einkaufswagen die Kunden animieren, mehr einzukaufen.<br>
  Für die Prüfung der Hypothese wurden Daten von %s Einkäufen in drei Filialen <em>Ost</em>, 
  <em>Nordwest</em>
  und <em>Süd</em> jeweils während 6 Wochen mit normalen Einkaufswagen (<em>L</em>) erfasst, und 6 Wochen mit 
  grossen Einkaufswagen (<em>XL</em>).<br>", n)
  
  return(d.dat)
  
}


