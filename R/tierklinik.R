
#' Futterbedarf einer Tierklinik
#'
#' Erzeugt Daten zu Hunderasse und verwendetem Futtertyp für die Planung einer
#' Tierklinik.
#'
#' @param n anzahl der zu erzeugenden Hunde.
#'
#' @return ein gelabelter `data.frame` mit Identifikation, Alter, Rasse und
#'   Futtertyp.
#'
#' @details Geeignete Verfahren sind Kontingenztafeln, bedingte Häufigkeiten
#' und der Chi-Quadrat-Unabhängigkeitstest.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.15 Chi-Quadrat-Tests
#' @export
tierklinik <- function(n=150) {
  
  d.set <- data.frame(id=sample(1000:9999, n), 
                      alter = sample(1:8, n, replace=TRUE),
                      # produce associated ordinal variables
                      rndPairs(n = n, r=0.65, prop = list(c(.3, .15, .35, .20), 
                                                          c(.35, .45, .2))))
  
  levels(d.set$x) <- c("Deutscher Schäferhund", "Terrier", 
                       "Rauhaardackel", "Dalmatiner")
  levels(d.set$y) <- c("Trockenfutter", "Dosenfutter", "Mischung")
  
  colnames(d.set)[3:4] <- c("rasse", "futter")
  bedrock::label(d.set, TRUE) <- c(
    "Identifikationsnummer",
    "Alter",
    "Hunderasse",
    "Benötigtes Futter")    
  
  bedrock::label(d.set,) <- gettextf("Eine Tierklinik ist auf die tierärztliche Versorgung der vier Hunderassen 
            %s spezialisiert. Die Tiere erhalten entweder Trockenfutter, 
            Dosenfutter oder eine Mischung von beidem, je nachdem was die 
            Besitzer verwenden. Für die Beschaffung des Futters will die Klinik prüfen, 
            ob der Futtertyp von der Hunderasse abhängt. 
            Wäre dies nicht der Fall, kann die Klinik das Futter nur auf der Grundlage 
            der Gesamtzahl der Hunde bestellen, ohne die Rassen zu berücksichtigen.
            Die Klinik sammelte über eine gewisse Zeit die Angaben zum Hund und zum Futter.
            ", paste(gettextf("<em>%s</em>", levels(d.set$rasse)), collapse=", "))
  
  return(d.set)
  
}


