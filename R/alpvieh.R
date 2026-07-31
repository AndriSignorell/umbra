

#' @export
alpvieh <- function(n=100){
  
  # n <- 100
  fuetterung <- factor(sample(c("keine", "heu", "kraftfutter"), n, replace = TRUE),
                       levels=c("keine", "heu", "kraftfutter"))
  alp <- rbinom(n, 1, 0.6)
  startgewicht <- round(rnorm(n, mean = 125, sd = 6), 1)
  alter <- round(rnorm(n, mean = 6, sd = 0.5), 1)
  geschlecht <- factor(c("m","w")[rbinom(n, 1, 0.5)+1], levels=c("m","w"))
  
  # Regressionsstruktur (wahre Effekte)
  beta0 <- 50
  beta_fuet <- c(keine = 0, Heu = 10, Kraftfutter = 25)
  beta_alp <- 5
  beta_start <- 0.6
  beta_alter <- 2
  
  schlachtgewicht <- round(
    beta0 +
      beta_fuet[fuetterung] +
      beta_alp * alp +
      beta_start * startgewicht +
      beta_alter * alter +
      rnorm(n, 0, 5), 
    1)
  
  d.set <- data.frame(schlachtgewicht, fuetterung, alp, startgewicht, alter, geschlecht)
  
  bedrock::label(d.set, TRUE) <- c("Schlachtgewicht in [kg]",
                                   "die Fütterungsart während der Sömmerung",
                                   "1 wenn das Kalb auf einer Alp gesömmert wurde",
                                   "das Startgewicht zu Beginn der Sömmerung in [kg]",
                                   "das Alter bei Beginn der Sömmerung (Monate)",
                                   "das Geschlecht des Kalbes")
  
  bedrock::label(d.set,) <- as.html("In einer Untersuchung sollte analysiert werden, welche Einflussgrössen 
  das Schlachtgewicht (Zielvariable) in [kg] von Kälbern nach der Sömmerung bestimmen. 
  Die Fütterungsart beschreibt, ob und in welchem Umfang während der Sömmerung 
  zugefüttert wurde. Es wird angenommen, dass zusätzliche Fütterung – insbesondere 
  mit Kraftfutter – zu einem höheren Schlachtgewicht führen könnte. Auch ein 
  Alpaufenthalt könnte sich positiv auswirken, da Kälber dort oft bessere 
  Weidebedingungen vorfinden.")
  
  return(d.set)
  
}

