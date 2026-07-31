
# Vegetarische Ernährung
#' Ernährungsform nach Grossverteiler
#'
#' Liefert Einzeldaten aus einer Kontingenztabelle zu Grossverteiler und
#' Ernährungsform.
#'
#' @return ein gelabelter `data.frame` mit Identifikation, Grossverteiler und
#'   Ernährungsform.
#'
#' @details Geeignete Verfahren sind Kontingenztafeln, bedingte Häufigkeiten
#' und der Chi-Quadrat-Unabhängigkeitstest.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.15 Chi-Quadrat-Tests
#' @export
vegi <- function(){
  
  # get base table
  m <- setNamesX(rbind("Migros" = c(196,22,5), 
                       "Coop" = c(202,30,11),
                       "Lidl" = c(153,13,2),
                       "Aldi" = c(140,9,2)
  ),
  colnames=c("Fleisch", "Vegetarisch", "Vegan")) 
  names(dimnames(m)) <- c("Grossverteiler", "Ernährung")
  
  d.set <- data.frame(id=seq(sum(m)), untable(m))
  
  bedrock::label(d.set,) <- bedrock::mGsub(c("&level_x&", "&level_y&"),
                                           c(paste(gettextf("<em>%s</em>", gsub("&nbsp;","", rownames(m))), collapse=", "),
                                             paste(gettextf("<em>%s</em>", colnames(m)), collapse=", ")),
                                           "Anhand von Einkaufsbelegen verschiedener Grossverteiler haben 
                         Forschende untersucht, wie viel 
                         Fleisch Schweizer Haushalte konsumieren. Es interessiert, ob es 
                         sich die Kundschaft der Grossverteiler sich in ihren 
                         Ernährungsgewohnheiten unterscheidet."
  ) 
  
  return(d.set)
  
}


