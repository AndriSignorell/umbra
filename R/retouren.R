
#' Garantiefälle elektronischer Geräte
#'
#' Erzeugt Verkaufsdaten mit Garantiefällen nach Hersteller und Produktgruppe.
#'
#' @param n ungefähre Gesamtzahl der zu erzeugenden Verkäufe.
#' @param p derzeit nicht ausgewertetes Kompatibilitätsargument.
#' @param name_x derzeit nicht ausgewertetes Kompatibilitätsargument.
#'
#' @return ein gelabelter `data.frame` mit Verkaufsnummer, Garantiefall,
#'   Hersteller und Produkt.
#'
#' @details Geeignete Verfahren sind relative Häufigkeiten, Binomialmodelle,
#' Konfidenzintervalle für Anteile und Chi-Quadrat-Tests.
#'
#' @concept 1.7 Grundlagen Wahrscheinlichkeit
#' @concept 1.8 Rechnen mit Wahrscheinlichkeiten
#' @concept 1.9 Zufallsvariablen und Verteilungen
#' @concept 1.10 Diskrete Verteilungen
#' @concept 1.12 Stichproben und Konfidenzintervalle
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.15 Chi-Quadrat-Tests
#' @export
retouren <- function(n=1248, p=0.016, name_x){
  
  hersteller <- function(n, p, name_x){
    
    data.frame(
      garantiefall = sample(c("nein", "ja"), size = n, replace = TRUE, 
                            prob = c(1-p, p)),
      hersteller = name_x,
      produkt    = sample(c("TV", "DVD-Player", "Beamer", "HiFi"), 
                          size = n, replace = TRUE)
      
    )
  }
  
  
  d.ctr <- data.frame(name_x = c("LG", "Samsung", "Sony","Panasonic"),
                      p = c(0.014, 0.015, 0.018, 0.02)*4,
                      n = round(c(.18, .33, .39, .10)*n))
  
  
  d.dat <- do.call(rbind, lapply(seq(nrow(d.ctr)), 
                                 function(i) with(d.ctr[i, ], 
                                                  hersteller(n, p, name_x)))  )
  
  d.dat$verkaufsnr <- round(runif(n=n)*1000+1000)
  
  bedrock::label(d.dat) <- "
    Ein Onlineanbieter elektronischer Geräte zählt die Garantiefälle einer Marke innerhalb 
    einer bestimmten Kategorie (Bsp. «TV»), die im After Sales abgewickelt werden müssen.
    So soll die Wahrscheinlichkeit für einen Garantiedefekt während der gesetzlichen 
    Garantiezeit transparent gemacht werden und auch der Vergleich zwischen  
    unterschiedlichen Herstellern ermöglicht werden. Ein Hersteller, der eventuell 
    mehr auf eine nachhaltige Bauweise setzt, ist so leichter zu erkennen.
    
    Es liegen die Verkaufsdaten zweier Jahre vor: 
    &nbsp;&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;&nbsp;. 
    "
  
  return(sampleX(d.dat[, c(4,1:3)]))
  
}


