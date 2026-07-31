
#' Schneehöhen in Schweizer Bergregionen
#'
#' `r .schneeMeta$description`
#'
#' @param n anzahl der zu erzeugenden Schneeproben.
#'
#' @return ein gelabelter `data.frame` mit folgenden Variablen:
#'
#' `r .rdVariables(.schneeMeta$variables)`
#'
#' @section Geeignete statistische Verfahren:
#' Der Datensatz eignet sich insbesondere für Streudiagramme, Korrelation,
#' Gruppenvergleiche sowie einfache und multiple lineare Regression.
#'
#' @concept Empirische Verteilungen
#' @concept Kennzahlen
#' @concept Bivariate Datenanalyse
#' @concept Einfache lineare Regression
#' @concept Multiple lineare Regression
#'
#' @export
schnee <- function(n = 507) {
  
  n <- .assertSampleSize(n, max = 9000L)
  
  res <- .simulateSchnee(n)
  
  bedrock::label(res, vars = TRUE) <-
    unname(.schneeMeta$variables)
  
  bedrock::label(res) <-
    .schneeMeta$description
  
  return(res)
}


# == internal helper functions ==========================================


.schneeMeta <- list(
  
  description = paste(
    "Die Schneehöhe in den Bergen hängt massgeblich von der",
    "Meereshöhe ab. Weiter spielt auch die Ausrichtung des Hangs",
    "eine Rolle. An Nordhängen entsteht typischerweise eine",
    "dickere Schneedecke."
  ),
  
  variables = c(
    proben_id = "Identifikationsnummer der Schneeprobe",
    meereshöhe = "Höhe des Messpunkts über Meer \\[m\\]",
    hanglage = "Ausrichtung des Hangs",
    kanton = "Kanton des Messpunkts",
    schneehöhe = paste(
      "Maximal gemessene Schneehöhe",
      "während der Messperiode [cm]"
    )
  ),
  
  concepts = c(
    "Empirische Verteilungen",
    "Kennzahlen",
    "Bivariate Datenanalyse",
    "Einfache lineare Regression",
    "Multiple lineare Regression"
  )
)



.simulateSchnee <- function(n) {
  
  ## Rechtsschiefe Höhenverteilung:
  ## ungefähr Mittelwert 1200 m und Standardabweichung 358 m
  meereshöhe <- round(
    400 + 2400 * rbeta(
      n,
      shape1 = 3,
      shape2 = 6
    ),
    digits = -1
  )
  
  ## Referenzkategorie: west/ost
  hanglage <- factor(
    sample(
      c("west/ost", "nord", "süd"),
      size = n,
      replace = TRUE,
      prob = c(0.30, 0.40, 0.30)
    ),
    levels = c("west/ost", "nord", "süd")
  )
  
  ## Referenzkanton: BE
  kanton <- factor(
    sample(
      c("BE", "GR", "TI", "UR", "VS"),
      size = n,
      replace = TRUE,
      prob = c(0.20, 0.37, 0.06, 0.14, 0.23)
    ),
    levels = c("BE", "GR", "TI", "UR", "VS")
  )
  
  ## Effekte der Hanglage in Zentimetern
  hangEffekt <- c(
    "west/ost" = 0,
    "nord" = 75,
    "süd" = -40
  )
  
  ## Kantonseffekte in Zentimetern
  kantonEffekt <- c(
    "BE" = 0,
    "GR" = 20,
    "TI" = -15,
    "UR" = 10,
    "VS" = -10
  )
  
  ## Erwartete maximale Schneehöhe
  erwartungswert <- 0.20 * meereshöhe +
    unname(hangEffekt[as.character(hanglage)]) +
    unname(kantonEffekt[as.character(kanton)])
  
  ## Beobachtete Schneehöhe mit zufälliger Streuung
  schneehöhe <- round(
    pmax(
      0,
      erwartungswert + rnorm(
        n,
        mean = 0,
        sd = 25
      )
    ),
    digits = 1
  )
  
  res <- data.frame(
    proben_id = sample(1000:9999, size = n),
    meereshöhe,
    hanglage,
    kanton,
    schneehöhe
  )
  
  return(res)
}

