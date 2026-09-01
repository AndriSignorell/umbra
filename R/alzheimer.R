
#' Erinnerungsleistung bei Alzheimer
#'
#' Erzeugt Fall- und Kontrolldaten zur Anzahl erinnerter Begriffe in einem
#' Gedächtnistest.
#'
#' `r .alzheimerMeta$description`
#'
#' @param n anzahl der zu erzeugenden Studienteilnehmenden.
#'
#' @return ein gelabelter `data.frame` mit folgenden Variablen:
#'
#' `r .rdVariables(.alzheimerMeta$variables)`
#'
#' @details
#' Die Anzahl erinnerter Begriffe wird als binomialverteilte Zufallsvariable
#' mit 30 möglichen Begriffen erzeugt. Für gesunde Kontrollpersonen beträgt
#' die Erinnerungswahrscheinlichkeit pro Begriff 0.40, für Personen der
#' Alzheimergruppe 0.15.
#'
#' Geeignete Verfahren sind Verteilungs- und Gruppenvergleiche,
#' Konfidenzintervalle, Signifikanztests sowie parametrische und
#' nichtparametrische Zweistichprobentests.
#'
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.12 Stichproben und Konfidenzintervalle
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.14 t-Tests
#' @concept 1.17 Nichtparametrische Tests
#'
#' @export
alzheimer <- function(n = 554) {
  
  n <- .assertSampleSize(
    n,
    max = 9000L
  )
  
  res <- .simulateAlzheimer(n)
  
  res <- .labelEmulData(
    res,
    meta = .alzheimerMeta
  )
  
  return(res)
}




# == internal helper functions ==========================================


.alzheimerMeta <- list(
  
  description = paste(
    "In einer Alzheimerstudie werden die Erinnerungsleistungen von",
    "Alzheimer-Patienten der Gruppe 'case' mit jenen von gesunden",
    "Kontrollpersonen der Gruppe 'control' verglichen. Den Teilnehmenden",
    "werden 30 Gegenst\u00e4nde vorgelegt, die sie anschliessend aus dem",
    "Ged\u00e4chtnis aufschreiben sollen."
  ),
  
  variables = c(
    id = "Identifikationsnummer der teilnehmenden Person",
    geschlecht = "Geschlecht der teilnehmenden Person",
    alter = "Alter der teilnehmenden Person \\[Jahre\\]",
    gruppe = "Studiengruppe mit den Auspr\u00e4gungen 'control' und 'case'",
    items = "Anzahl erinnerter Begriffe im Ged\u00e4chtnistest"
  )
)




.simulateAlzheimer <- function(n) {
  
  ## Geschlecht
  geschlecht <- factor(
    sample(
      c("m", "w"),
      size = n,
      replace = TRUE,
      prob = c(0.37, 0.63)
    ),
    levels = c("m", "w")
  )
  
  ## Alter: linksschief mit einer H\u00e4ufung bei h\u00f6heren Alterswerten
  alter <- round(
    bedrock::rBetaShape(
      n,
      shape = "left",
      bounds = c(58, 87)
    )
  )
  
  ## Studiengruppe mit 'control' als Referenzkategorie
  gruppe <- factor(
    sample(
      c("control", "case"),
      size = n,
      replace = TRUE,
      prob = c(0.55, 0.45)
    ),
    levels = c("control", "case")
  )
  
  ## Wahrscheinlichkeit, einen einzelnen Begriff zu erinnern
  erinnerungsWahrscheinlichkeit <- c(
    control = 0.40,
    case = 0.15
  )
  
  ## Anzahl erinnerter Begriffe von insgesamt 30
  items <- rbinom(
    n,
    size = 30,
    prob = erinnerungsWahrscheinlichkeit[as.character(gruppe)]
  )
  
  res <- data.frame(
    id = sample(1000:9999, size = n),
    geschlecht,
    alter,
    gruppe,
    items
  )
  
  return(res)
}


