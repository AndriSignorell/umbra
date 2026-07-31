
#' Autismus und Stilldauer
#'
#' Liefert eine Kontingenztabelle zu Autismusstatus und Stilldauer.
#'
#' @param noise logischer Wert; bei `TRUE` werden die Zellhäufigkeiten leicht
#'   zufällig verändert.
#'
#' @return eine gelabelte Häufigkeitsmatrix.
#'
#' @details Geeignete Verfahren sind bedingte Häufigkeiten und der
#' Chi-Quadrat-Unabhängigkeitstest.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.15 Chi-Quadrat-Tests
#' @export
autismus <- function(noise=FALSE){
  
  # Erstellen der Kontingenztabelle
  dat <- matrix(
    c(241, 198, 164, 215,   # Row: Autism Yes
      20,  25,  27,  44    # Row: Autism No
    ), # Totals
    nrow = 2, byrow = TRUE, 
    dimnames=list( 
      Autismus  = c("Ja", "Nein"),
      Stillzeit = c("0 Monate", "<2 Monate", "2-6 Monate", 
                    ">6 Monate")))
  
  if(noise)
    dat <- round(jitter(dat, amount = 4))
  
  bedrock::label(dat) <- "Gibt es einen Zusammenhang zwischen Autismus und Stillen? Um dies 
      festzustellen, wurden Mütter von autistischen und nicht-autistischen 
      Kindern befragt, ob und wenn ja bis zu welchem Zeitpunkt sie ihre Kinder gestillt haben. 
      Die Daten sind in folgender Tabelle dargestellt. Bieten die Daten 
      genügend Hinweise darauf, dass Stillen und Autismus unabhängig 
      voneinander sind?"
  
  return(dat)
  
}


