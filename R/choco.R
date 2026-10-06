
#' Schokoladenkonsum nach Sprachregion
#'
#' Erzeugt Umfragedaten zum monatlichen Schokoladenkonsum in drei
#' Sprachregionen der Schweiz.
#'
#' @param n anzahl der zu erzeugenden Personen.
#'
#' @return ein gelabelter `data.frame` mit soziodemografischen Merkmalen sowie
#'   numerischem und kategorisiertem Konsum.
#'
#' @details Geeignete Verfahren sind empirische Verteilungen, Kennzahlen,
#' bivariate Analysen, Chi-Quadrat-Tests und Varianzanalyse.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.15 Chi-Quadrat-Tests
#' @concept 1.16 Varianzanalyse
#' @export
choco <- function(n=300) {
  
  d.set <- data.frame(id=sample(1000:9999, n), 
                      geschlecht=sample(c("m","w"), n, replace=TRUE, prob=c(0.37, .73)),
                      alter = sample(18:75, n, replace=TRUE),
                      region= region <- sample(c("de","fr","it"), n, replace=TRUE, 
                                               prob=c(0.14, 0.24, .62)),
                      menge=menge <- rpois(n, lambda = c("de"=1.8,"it"=3,"fr"=2.2)[region]),
                      mengekat= cut(menge, breaks=c(0,1,3,5, Inf), right=FALSE, 
                                    labels = c("0", "1-2", "3-4", "5.."))
  )
  
  bedrock::label(d.set, TRUE) <- c("ID", "Geschlecht", "Alter", 
                                   "Wohnregion", "Bezogene Packungen Schokolade", 
                                   "Mengen-Kategorie")
  
  bedrock::label(d.set,) <- "
          Ein Schweizer Detailhändler gibt eine Umfrage in Auftrag. Es soll 
          herausgefunden werden, ob die Absatzmenge einer bestimmten Schokoladensorte 
          durch die regional unterschiedlichen ökonomischen, sozialen und 
          kulturellen Gegebenheiten beeinflusst wird. Dazu wurden Personen 
          in verschiedenen Sprachregionen der Schweiz gefragt, wie viele 
          Packungen der betreffenden Sorte sie pro Monat kaufen."
  
  
  # Die entsprechende Variable <strong>mengekat</strong> hat vier 
  # Ausprägungen <em>"0", "1-2", "3-4"</em> und <em>"5.."</em> (letzere 
  # 5 oder mehr Packungen). 
  
  
  return(d.set)
  
}

