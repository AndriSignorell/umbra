
#' Schlachtgewicht von Rindern
#'
#' Liefert Produktionsdaten zu Schlachtgewicht, Alter, Rasse, Futter und
#' Sömmerung von Rindern.
#'
#' @param n vorgesehene Anzahl Beobachtungen; die effektive Anzahl wird durch
#'   den hinterlegten Basisdatensatz bestimmt.
#'
#' @return ein gelabelter `data.frame` mit den Produktionsmerkmalen.
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
rinder <- function(n=554) {
  
  # Schlachtalter:  18-24 Monate
  # Schlachtgewicht: 334 kg
  
  d.set <- .packageData("rinder.xlsx")
  
  d.set$alter <- round(rnorm(nrow(d.set), mean = 21, sd=2), 0)
  d.set$gewicht <- round(c(0,-23,-30)[nf(d.set$rasse)] + 
                           + 0.4* d.set$alter + 
                           + c(0,+30,+15)[nf(d.set$futter)] + 
                           + c(0,-23)[nf(d.set$sömmerung)] + 
                           + 290 + rnorm(nrow(d.set), 0, 5), 0)
  d.set$rasse <- factor(d.set$rasse)
  d.set$futter <- factor(d.set$futter)
  d.set$sömmerung <- factor(d.set$sömmerung)
  
  bedrock::label(d.set, TRUE) <- c("Schlachtgewicht des Rinds in [kg]", "Alter des Rinds [in Monaten]", 
                                   "Rasse des Rinds", "Futtertyp", "Lage der Sömmerung")
  
  bedrock::label(d.set,) <- "
          Für die Fleischproduktion werden typischerweise diverse Rinderrassen verwendet, 
          die mit 
          verschiedenenartigem Futter ernährt werden. Sowohl die Rasse, das 
          Futter, die Art der
          Sömmerung (wo sich die Tiere im Sommer aufhalten) aber auch 
          das Alter der Rinder haben einen Einfluss auf das Schlachtgewicht.  
          Der Datensatz&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;enthält 
          Daten, die bei einem
          Fleischproduzenten in einem Produktionszyklus erfasst wurden."
  
  return(d.set)
  
  
}



