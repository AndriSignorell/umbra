
#' Bruttoinlandprodukt der grössten Volkswirtschaften
#'
#' Liefert das Bruttoinlandprodukt der nach BIP grössten Volkswirtschaften im
#' Jahr 2023.
#'
#' @param n anzahl der grössten Volkswirtschaften, die zurückgegeben werden.
#'
#' @return ein gelabelter `data.frame` mit Land und Bruttoinlandprodukt.
#'
#' @details Geeignete Verfahren sind Rangordnungen, Kennzahlen, relative
#' Anteile, Lorenzkurve und Konzentrationsmasse.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.6 Konzentration
#' @export
bip <- function(n = 20){
  
  d.set <- .packageData("bip.xlsx")[1:n, ]
  
  bedrock::label(d.set, TRUE) <- c("Land",     
                                   "Bruttoinlandprodukt 2023"
  )
  
  bedrock::label(d.set,) <- gettextf('
        Das Bruttoinlandprodukt (BIP) misst den Gesamtwert aller 
        Waren und Dienstleistungen, die in einem Land in einem bestimmten 
        Zeitraum produziert werden. Der vorliegende Datensatz umfasst das BIP für die
        %s grössten Volkswirtschaften weltweit im Jahr 2023 (in Milliarden US-Dollar).',
                                     n)
  
  return(d.set)
  
}



