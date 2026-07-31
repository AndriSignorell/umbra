
#' Schäden in der Hausratversicherung
#'
#' Erzeugt eine Mischung aus häufigen kleinen und seltenen grossen
#' Versicherungsschäden.
#'
#' @param n anzahl der zu erzeugenden Schäden.
#' @param mu_small mittelwert der normalverteilten kleinen Schäden.
#' @param sd_small standardabweichung der normalverteilten kleinen Schäden.
#' @param scale skalenparameter der Pareto-verteilten grossen Schäden.
#' @param shape formparameter der Pareto-verteilten grossen Schäden.
#'
#' @return ein gelabelter numerischer Vektor mit gerundeten Schadenhöhen.
#'
#' @details Geeignete Verfahren sind empirische Verteilungen, robuste
#' Kennzahlen, Konzentrationsmasse, Quantile und stetige Verteilungsmodelle.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.6 Konzentration
#' @concept 1.9 Zufallsvariablen und Verteilungen
#' @concept 1.11 Kontinuierliche Verteilungen
#' @export
haushaltschaden <- function(n, mu_small=1000, sd_small=500, 
                            scale=2500, shape=2.5){
  # example:
  
  # haushaltschaden(n=1000, shape=2)
  
  rpareto <- function (n, scale = 1, shape) 
  {
    ans <- scale/runif(n)^(1/shape)
    ans[scale <= 0] <- NaN
    ans[shape <= 0] <- NaN
    ans
  }
  
  small_damages <- abs(rnorm(n, mean = mu_small, sd = sd_small))
  large_damages <- rpareto(n, scale = scale, shape = shape)
  
  # Kombinierte Verteilung (80% kleine, 20% grosse Schäden)
  weights <- sample(c(TRUE, FALSE), size = n, replace = TRUE, prob = c(0.8, 0.2))
  damages <- ifelse(weights, 
                    sample(small_damages, size = n, replace = TRUE), 
                    sample(large_damages, size = n, replace = TRUE))
  
  bedrock::label(damages) <- "Versicherungen verzeichnen häufig eine hohe Frequenz 
               kleiner Schäden und eine seltene, aber hohe Schadenslast durch 
               Extremereignisse. Hausratversicherungen in der Schweiz decken 
               übliche Gefahren wie Diebstahl, Feuer und Wasser ab, wobei 
               kleine Schäden oft einfach reguliert werden, während grosse
               Schäden besondere Versicherungsfälle sind."
  
  return(round(damages, 0))
  
  
}



