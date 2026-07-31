
#' Weiterempfehlungsbereitschaft von Möbelkunden
#'
#' Liefert Umfragedaten zur Weiterempfehlungsbereitschaft nach Alter,
#' Geschlecht und Wohnregion.
#'
#' @return ein gelabelter `data.frame` mit Kundenmerkmalen und Bewertung.
#'
#' @details Geeignete Verfahren sind Skalenbestimmung, empirische
#' Verteilungen, Kennzahlen und bivariate Gruppenvergleiche.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @export
kunden <- function(){
  
  d.set <- .packageData("kunden.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Identifikationsnummer",     
                                   "Alter der Person in [Jahren]",
                                   "Geschlecht der Person",
                                   "Wohnregion",        
                                   "Weiterempfehlungsbereitschaft"
  )
  
  bedrock::label(d.set,) <- '
        Für ein Möbelgeschäft wurde von einem Institut eine Kundenumfrage durchgeführt. Dabei wurden 
        in einer Stichprobe die Variablen <em>alter, geschlecht, wohnregion</em> erfragt. 
        Danach wurde die Frage:<br><br>
        <em>"Wie wahrscheinlich auf einer Skala von 1 (unwahrscheinlich) - 10 (sicher) 
        ist es, dass Sie uns weiterempfehlen werden?"</em><br><br>
        gestellt. <br>Bewertungen zwischen 1 und 6 werden als <em>"detraktor"</em>, 7 und 8 
        als <em>"passiv"</em> und 9-10 als <em>"promotor"</em> interpretiert.'
  
  return(d.set)
}

