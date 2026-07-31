
#' Bevorzugte Sauce nach Geschlecht
#'
#' Liefert Einzeldaten aus einer Kontingenztabelle zu Geschlecht und
#' bevorzugter Sauce.
#'
#' @return ein gelabelter `data.frame` mit Identifikation, Geschlecht und
#'   Sauce.
#'
#' @details Geeignete Verfahren sind Kontingenztafeln, bedingte Häufigkeiten
#' und der Chi-Quadrat-Unabhängigkeitstest.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.15 Chi-Quadrat-Tests
#' @export
hotdog <- function(){
  
  # get base table
  m <- setNamesX(rbind("männlich" = c(53,46,15), 
                       "weiblich" = c(37,72,14)
  ),
  colnames=c("Senf", "Ketchup","Mayonnaise")) 
  names(dimnames(m)) <- c("Geschlecht", "Sauce")
  
  d.set <- data.frame(id=seq(sum(m)), untable(m))
  
  bedrock::label(d.set,) <- bedrock::mGsub(c("&level_x&", "&level_y&"),
                                           c(paste(gettextf("<em>%s</em>", gsub("&nbsp;","", rownames(m))), collapse=", "),
                                             paste(gettextf("<em>%s</em>", colnames(m)), collapse=", ")),
                                           "Die Betreiberin einer Würstchenbude möchte feststellen, ob es 
           einen Zusammenang zwischen dem Geschlecht ihrer Kundschaft (&level_x&) 
           und der bevorzugten 
           Sauce (&level_y&) gibt. Sie führt über eine Woche lang detailliert Buch über 
           ihre Verkäufe."
  ) 
  
  return(d.set)
  
}


