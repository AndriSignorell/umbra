

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
                                   "die F\u00fctterungsart w\u00e4hrend der S\u00f6mmerung",
                                   "1 wenn das Kalb auf einer Alp ges\u00f6mmert wurde",
                                   "das Startgewicht zu Beginn der S\u00f6mmerung in [kg]",
                                   "das Alter bei Beginn der S\u00f6mmerung (Monate)",
                                   "das Geschlecht des Kalbes")
  
  bedrock::label(d.set,) <- as.html("In einer Untersuchung sollte analysiert werden, welche Einflussgr\u00f6ssen 
  das Schlachtgewicht (Zielvariable) in [kg] von K\u00e4lbern nach der S\u00f6mmerung bestimmen. 
  Die F\u00fctterungsart beschreibt, ob und in welchem Umfang w\u00e4hrend der S\u00f6mmerung 
  zugef\u00fcttert wurde. Es wird angenommen, dass zus\u00e4tzliche F\u00fctterung - insbesondere 
  mit Kraftfutter - zu einem h\u00f6heren Schlachtgewicht f\u00fchren k\u00f6nnte. Auch ein 
  Alpaufenthalt k\u00f6nnte sich positiv auswirken, da K\u00e4lber dort oft bessere 
  Weidebedingungen vorfinden.")
  
  return(d.set)
  
}

