
#' Nutzung sozialer Medien
#'
#' Erzeugt Umfragedaten zur bevorzugten Social-Media-Anwendung nach
#' Geschlecht, Alter und Wohnregion.
#'
#' @param n anzahl der zu erzeugenden Personen.
#'
#' @return ein gelabelter `data.frame` mit soziodemografischen Merkmalen und
#'   bevorzugter Anwendung.
#'
#' @details Geeignete Verfahren sind Häufigkeitstabellen, bivariate Analysen
#' und Chi-Quadrat-Tests.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.15 Chi-Quadrat-Tests
#' @export
socialmedia <- function(n = 1224){
  
  dat <- data.frame(id=sample(1000:9999, n), 
                    region=sample(c("land","stadt"), n, replace=TRUE, prob=c(0.37, .73)),
                    alter = sample(16:60, n, replace=TRUE),
                    # produce associated ordinal variables
                    rndPairs(n = n, r=0.8, prop = list(c(.3, .15, .3, .25), 
                                                         c(.45, .55))))
  
  levels(dat$x) <- c("FaceBook", "LinkedIn", "WhatsApp", "Instagram")
  levels(dat$y) <- c("m","w")
  colnames(dat)[4:5] <- c("app", "geschlecht")
  dat <- dat[, c("id","geschlecht","alter","region","app")]
  
  dat$generation <- DescToolsX::generation(2024 - dat$alter)
  dat$online <- round(2+bedrock::num(dat$generation) * 0.9 + 
                        2*(dat$geschlecht=="w") + rnorm(n=nrow(dat), sd=1), 1)
  
  
  bedrock::label(dat, TRUE) <- c(
    "Identifikationsnummer",
    "Geschlecht der Person",
    "Alter",
    "Wohnregion (stadt/land)",
    "Am h\u00e4ufigsten verwendete Social Media Applikation",
    "Generation",
    "Online Zeit")    
  
  
  bedrock::label(dat,) <- "In dieser Studie 
        wurde untersucht, welche Social Media Applikationen von 
        unterschiedlichen Zielgruppen genutzt werden. "
  
  return(dat)
  
}


