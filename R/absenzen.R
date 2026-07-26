

## Fiktive Lehrdaten:
## Absenzen im Betrieb
absenzen <- function(seed=260725) {
    
  set.seed(seed)
  
  n <- 240L
  
  ## Zeiträume mit gleich vielen Beobachtungen
  zeitraum <- factor(
    sample(rep(c("Frühjahr", "Herbst"), each = n / 2)),
    levels = c("Frühjahr", "Herbst")
  )
  
  ## Unternehmensbereiche
  bereich <- factor(
    sample(
      c("Verwaltung", "Verkauf", "Logistik"),
      size = n,
      replace = TRUE,
      prob = c(0.25, 0.35, 0.40)
    ),
    levels = c("Verwaltung", "Verkauf", "Logistik")
  )
  
  ## Typische Teamgrössen unterscheiden sich nach Bereich
  muTeam <- unname(
    c(
      Verwaltung = 10,
      Verkauf = 15,
      Logistik = 24
    )[as.character(bereich)]
  )
  
  teamgroesse <- as.integer(
    pmin(
      pmax(rpois(n, lambda = muTeam - 5) + 5, 5),
      40
    )
  )
  
  ## Temporär Beschäftigte sind in der Logistik und im Herbst häufiger
  pTemp <- unname(
    c(
      Verwaltung = 0.06,
      Verkauf = 0.10,
      Logistik = 0.16
    )[as.character(bereich)]
  )
  
  pTemp <- pTemp + 0.04 * (zeitraum == "Herbst")
  
  anzahlTemporaere <- rbinom(
    n,
    size = teamgroesse,
    prob = pmin(pTemp, 0.35)
  )
  
  ## Durchschnittsalter der Teams
  muAlter <- unname(
    c(
      Verwaltung = 43,
      Verkauf = 37,
      Logistik = 39
    )[as.character(bereich)]
  )
  
  durchschnittsalter <- round(
    pmin(
      pmax(rnorm(n, mean = muAlter, sd = 5), 25),
      58
    ),
    1
  )
  
  ## Durchschnittliches Arbeitspensum in Prozent
  muPensum <- unname(
    c(
      Verwaltung = 84,
      Verkauf = 78,
      Logistik = 91
    )[as.character(bereich)]
  )
  
  durchschnittspensum <- round(
    pmin(
      pmax(rnorm(n, mean = muPensum, sd = 6), 55),
      100
    ),
    1
  )
  
  ## Erwartete Absenztage je Vollzeitstelle während drei Monaten
  ##
  ## Höhere Werte entstehen bei:
  ## - Beobachtung im Herbst,
  ## - höherem Durchschnittsalter,
  ## - grösseren Teams,
  ## - mehr temporär Beschäftigten.
  erwartung <- 1.8 +
    1.10 * (zeitraum == "Herbst") +
    0.055 * (durchschnittsalter - 40) +
    0.045 * (teamgroesse - 15) +
    0.22 * anzahlTemporaere
  
  ## Individuelle, nicht durch die berücksichtigten Merkmale erklärte Streuung
  absenztage <- round(
    pmax(
      0,
      erwartung + rnorm(n, mean = 0, sd = 0.70)
    ),
    1
  )
  
  ## Teams mit mindestens vier Absenztagen je Vollzeitstelle
  hoheAbsenz <- factor(
    ifelse(absenztage >= 4, "ja", "nein"),
    levels = c("nein", "ja")
  )
  
  ## Endgültiger Datensatz mit acht Variablen
  res <- data.frame(
    zeitraum,
    bereich,
    teamgroesse,
    anzahlTemporaere,
    durchschnittsalter,
    durchschnittspensum,
    absenztage,
    hoheAbsenz
  )
  
  label(res, vars = TRUE) <- c(
    "Beobachtungsperiode: Frühjahr oder Herbst",
    "Unternehmensbereich des Teams",
    "Anzahl der Mitarbeitenden im Team",
    "Anzahl der temporär Beschäftigten im Team",
    "durchschnittliches Alter der Teammitglieder in Jahren",
    "durchschnittliches Arbeitspensum der Teammitglieder in Prozent",
    "Absenztage je Vollzeitstelle während drei Monaten",
    "mindestens vier Absenztage je Vollzeitstelle: ja oder nein"
  )
  
  # story
  label(res) <- "In einer grösseren Schweizer Handelsfirma wurde eine
    Zunahme der Absenztage beobachtet. Das HR untersuchte 240 verschiedene
    Teams aus Verwaltung, Verkauf und Logistik. Jedes Team wurde während
    einer dreimonatigen Periode im Frühjahr oder im Herbst betrachtet.
    Die Absenztage wurden auf eine Vollzeitstelle standardisiert.
    Im Herbst vor dem Jahresabschluss war die Arbeitsbelastung höher.
    Als mögliche Einflussgrössen wurden die Teamgrösse, die Anzahl
    temporär Beschäftigter, das Durchschnittsalter und der Zeitraum
    untersucht."
  
  res
  
}

