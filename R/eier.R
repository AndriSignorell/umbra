
#' Gewicht und Qualität von Eiern
#'
#' Liefert Produktionsdaten zu Gewicht und Farbton von Eiern sowie Alter,
#' Rasse und Futter der Hühner.
#'
#' @param vars namen der Variablen, die zurückgegeben werden sollen.
#'
#' @return ein gelabelter `data.frame` mit den ausgewählten Variablen.
#'
#' @details Geeignete Verfahren sind Gruppenvergleiche, Varianzanalyse sowie
#' einfache und multiple lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.16 Varianzanalyse
#' @concept 1.18 Einfache lineare Regression
#' @concept 1.19 Multiple lineare Regression
#' @export
eier <- function(vars=c("gewicht","alter","rasse","futter","farbe")){
  
  d.set <- .packageData("eier.xlsx", stringsAsFactors = TRUE)
  
  
  bedrock::label(d.set, TRUE) <- c("Gewicht des Eis in [g]", "Alter des Huhns in Tagen", 
                                   "Rasse des Huhns", "Futtertyp", "Farbton des Eigelbs")
  
  d.set <- d.set[, vars]
  
  bedrock::label(d.set,) <- gettextf('
            Für die Produktion von Eiern werden typischerweise diverse Huhnrassen 
            verwendet, die mit 
            verschiedenenartigem Futter ernährt werden. Sowohl die Rasse, 
            das Futter, aber auch das Alter 
            der Hühner haben einen Einfluss auf das Gewicht und die Qualität 
            der Eier. Eier mit 
            dunkelgelbem Dotter werden von den Konsumenten gemeinhin als 
            irgendwie "gesünder" wahrgenommen. 
            Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;enthält 
            Daten, die bei einem
            Eierproduzenten in einem Produktionszyklus erfasst wurden.')
  
  return(d.set)
  
}


