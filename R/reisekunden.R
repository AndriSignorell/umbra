
# ---------------------------------------------------
# Funktion zur zufälligen Kundengenerierung
# ---------------------------------------------------
#' Zielgruppen eines Reisebüros
#'
#' Erzeugt Kundendaten zu Zielgruppe, Reiseziel und Reisemotivation.
#'
#' @param n anzahl der zu erzeugenden Kunden.
#'
#' @return ein gelabelter `data.frame` mit Kunden-ID, Geschlecht, Alter,
#'   Zielgruppe, Reiseziel und Motivation.
#'
#' @details Geeignete Verfahren sind Skalenbestimmung, Häufigkeitstabellen,
#' bivariate Analysen und Chi-Quadrat-Tests.
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.15 Chi-Quadrat-Tests
#' @export

reisekunden <- function(n = 500) {
  
  # ---------------------------------------------------
  # 🧳 Simulation eines Kundendatensatzes für ein Reisebüro
  # ---------------------------------------------------
  
  set.seed(123)  # für Reproduzierbarkeit
  
  # Beispielhafte Zielgruppen mit Wahrscheinlichkeiten
  zielgruppen <- c("Frauen", "Männer", "Jüngere (<30)", "Mittlere (30–50)", "Ältere (60+)")
  p_zielgruppen <- c(0.25, 0.25, 0.20, 0.20, 0.10)
  
  # Basis-Tabelle mit typischen Reisedestinationen und Motivationen
  reiseideen <- data.frame(
    Zielgruppe = c(
      "Frauen", "Frauen", "Männer", "Männer",
      "Jüngere (<30)", "Mittlere (30–50)", "Ältere (60+)"
    ),
    Reiseziel = c(
      "Bali", "Südfrankreich", "Graubünden", "Kanada",
      "Ibiza", "Südafrika", "Donaukreuzfahrt"
    ),
    Motivation = c(
      "Wellness & Natur", "Erholung & Kulinarik",
      "Berge & Sport", "Abenteuer & Roadtrip",
      "Party & Sonne", "Familienzeit & Komfort",
      "Kultur & Erholung"
    ),
    stringsAsFactors = FALSE
  )
  
  
  # Zufällige Zielgruppe pro Kunde
  gruppe <- sample(zielgruppen, n, replace = TRUE, prob = p_zielgruppen)
  
  # Geschlecht aus Zielgruppe ableiten
  geschlecht <- ifelse(gruppe == "Frauen", "weiblich",
                       ifelse(gruppe == "Männer", "männlich",
                              sample(c("weiblich", "männlich"), n, replace = TRUE)))
  
  # Alter je nach Gruppe simulieren
  alter <- sapply(gruppe, function(g) {
    if (g == "Jüngere (<30)") rnorm(1, 25, 3)
    else if (g == "Mittlere (30–50)") rnorm(1, 40, 5)
    else if (g == "Ältere (60+)") rnorm(1, 68, 4)
    else rnorm(1, 45, 10)
  })
  
  # Reiseziel & Motivation passend zur Zielgruppe
  reisedaten <- sapply(gruppe, function(g) {
    sample(reiseideen$Reiseziel[reiseideen$Zielgruppe == g], 1)
  })
  
  motive <- sapply(gruppe, function(g) {
    sample(reiseideen$Motivation[reiseideen$Zielgruppe == g], 1)
  })
  
  # Zusammenführen
  daten <- data.frame(
    KundenID = 1:n,
    Geschlecht = geschlecht,
    Alter = round(alter),
    Zielgruppe = gruppe,
    Reiseziel = reisedaten,
    Motivation = motive,
    stringsAsFactors = FALSE
  )
  
  
  bedrock::label(daten, TRUE) <- "Ein Reisebüro will die Unterschiede in den Reisepräferenzen 
          nach Geschlecht und Altersgruppe abbilden, wie sie typischerweise 
          in Marktanalysen beobachtet werden."
  
  bedrock::label(daten) <- c("die ID des Kunden", "das Alter", "die Zielgruppe", "das letzte Reiseziel",
                             "die geäusserte Motivation")
  
  return(daten)
  
}