
#' Jagddaten zu Gämsen
#'
#' Liefert Messdaten zu erlegten Gämsen einschliesslich Alter, Gewicht,
#' Kruckenlänge und Jagdgebiet.
#'
#' @return ein gelabelter `data.frame` mit den Merkmalen der erlegten Tiere.
#'
#' @details Geeignete Verfahren sind empirische Verteilungen, Kennzahlen,
#' Gruppenvergleiche, nichtparametrische Tests und lineare Regression.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.17 Nichtparametrische Tests
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
gams <- function() {
  
  d.set <- .packageData("gemsen.xlsx", stringsAsFactors = TRUE)
  
  d.set <- na.omit(d.set)
  d.set$banngebiet <- relevel(factor(d.set$ref, labels=c("ja", "nein")), ref="nein")
  d.set <- d.set[, c("jahr","geschlecht","alter","gewicht","krucken","banngebiet")]
  
  bedrock::label(d.set, TRUE) <- c("Abschussjahr",
                                   "Geschlecht des Tieres",
                                   "Alter des erlegten Tieres",
                                   "Gewicht des erlegten Tieres in [kg]",
                                   "Kruckenlänge (Hornlänge) in [mm]",
                                   "Abschussgebiet ist Jagdbanngebiet"
  )
  
  bedrock::label(d.set,) <- gettextf("          
                   Zur Beurteilung des Zustands der Gamspopulation wurden im Jagdinspektorat 
                   die in der regulären Jagd erlegten Gämsen vermessen.")
  
  return(d.set)
  
  
}

