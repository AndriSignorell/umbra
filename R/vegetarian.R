

set.seed(123)

n <- 10000

# -----------------------------
# Basisvariablen
# -----------------------------
data <- data.frame(
  id = 1:n,
  sex = sample(c("female", "male"), n, replace = TRUE, prob = c(0.5, 0.5)),
  age_group = sample(c("18-35", "36-60", "60+"), n, replace = TRUE, 
                     prob = c(0.35, 0.45, 0.20)),
  residence = sample(c("urban", "rural"), n, replace = TRUE, 
                     prob = c(0.7, 0.3))
)

# -----------------------------
# Funktion zur Wahrscheinlichkeitsanpassung
# -----------------------------
get_probs <- function(sex, age_group, residence) {
  
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

# -----------------------------
# Ernährung simulieren
# -----------------------------
data$diet <- mapply(function(sex, age_group, residence) {
  probs <- get_probs(sex, age_group, residence)
  sample(names(probs), 1, prob = probs)
}, data$sex, data$age_group, data$residence)

# -----------------------------
# Ergebnis checken
# -----------------------------
prop.table(table(data$diet))

# nach Gruppen
prop.table(table(data$diet, data$sex), 2)
prop.table(table(data$diet, data$age_group), 2)
prop.table(table(data$diet, data$residence), 2)




