
# Draft of a further data set: diet by sex, age group and residence -------
#
# Not exported yet. This used to be a script at the top level of the file:
# it ran when the package was built, set a seed there, and left its objects
# `n`, `data` and `get_probs` in the namespace -- where `n` silently served
# every generator that had forgotten to declare it. The data set of the
# script is bedrock::withSeed(123, vegetarian()).

vegetarian <- function(n = 10000) {
  
  # -----------------------------
  # Basisvariablen
  # -----------------------------
  res <- data.frame(
    id = 1:n,
    sex = sample(c("female", "male"), n, replace = TRUE, prob = c(0.5, 0.5)),
    age_group = sample(c("18-35", "36-60", "60+"), n, replace = TRUE, 
                       prob = c(0.35, 0.45, 0.20)),
    residence = sample(c("urban", "rural"), n, replace = TRUE, 
                       prob = c(0.7, 0.3))
  )
  
  # -----------------------------
  # Ernährung simulieren
  # -----------------------------
  res$diet <- mapply(function(sex, age_group, residence) {
    probs <- .dietProbs(sex, age_group, residence)
    sample(names(probs), 1, prob = probs)
  }, res$sex, res$age_group, res$residence)
  
  res
  
  # Ergebnis checken:
  #   prop.table(table(res$diet))
  # nach Gruppen:
  #   prop.table(table(res$diet, res$sex), 2)
  #   prop.table(table(res$diet, res$age_group), 2)
  #   prop.table(table(res$diet, res$residence), 2)
}


# -----------------------------
# Funktion zur Wahrscheinlichkeitsanpassung
# -----------------------------
.dietProbs <- function(sex, age_group, residence) {
  
  # Basis
  p <- c(
    omnivore = 0.72,
    flexitarian = 0.22,
    vegetarian = 0.05,
    vegan = 0.01
  )
  
  # Geschlecht
  if (sex == "female") {
    p["vegetarian"] <- p["vegetarian"] * 1.5
    p["vegan"] <- p["vegan"] * 1.8
    p["omnivore"] <- p["omnivore"] * 0.9
  }
  
  # Alter
  if (age_group == "18-35") {
    p["vegetarian"] <- p["vegetarian"] * 1.5
    p["vegan"] <- p["vegan"] * 1.5
    p["flexitarian"] <- p["flexitarian"] * 1.3
  }
  
  if (age_group == "60+") {
    p["vegetarian"] <- p["vegetarian"] * 0.5
    p["vegan"] <- p["vegan"] * 0.3
    p["omnivore"] <- p["omnivore"] * 1.2
  }
  
  # Wohnort
  if (residence == "urban") {
    p["vegetarian"] <- p["vegetarian"] * 1.4
    p["vegan"] <- p["vegan"] * 1.6
    p["flexitarian"] <- p["flexitarian"] * 1.2
  }
  
  if (residence == "rural") {
    p["omnivore"] <- p["omnivore"] * 1.2
  }
  
  # Normieren
  p <- p / sum(p)
  
  return(p)
}
