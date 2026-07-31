
#' Reaktionsdauer von Bank-Personalabteilungen
#'
#' Liefert Daten zur mittleren Reaktionsdauer von Personalabteilungen in
#' Abhängigkeit von der Anzahl Angestellten.
#'
#' @return ein gelabelter `data.frame` mit Bank, Anzahl Angestellten und
#'   Reaktionsdauer.
#'
#' @details Geeignete Verfahren sind Streudiagramm, Korrelation und einfache
#' lineare Regression.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.18 Einfache lineare Regression
#' @export
bankhr <- function(){
  
  d.set <- .packageData("bankhr.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Name der Bank", 
                                   "Anzahl Angestellte",
                                   "Mittlere Reaktionsdauer [in Tagen]"
  )
  
  bedrock::label(d.set,) <- "
              Um die Personalabteilungen von Banken zu vergleichen, wurde in einer 
              experimentellen Studie &n& Banken jeweils 4 Bewerbungen zugestellt und 
              die mittlere Anzahl Tage festgehalten, die zwischen dem Einreichen 
              der Bewerbung und der Reaktion lag. Vermutet wird, dass die 
              Reaktionsgeschwindigkeit von
              der Grösse der Bank (Anzahl Angestellten) abhängen könnte.<br>
              "
  
  return(d.set)
  
}
