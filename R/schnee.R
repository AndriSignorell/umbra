
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
    "Die Schneeh\u00f6he in den Bergen h\u00e4ngt massgeblich von der",
    "Meeresh\u00f6he ab. Weiter spielt auch die Ausrichtung des Hangs",
    "eine Rolle. An Nordh\u00e4ngen entsteht typischerweise eine",
    "dickere Schneedecke."
  ),
  
  variables = c(
    proben_id = "Identifikationsnummer der Schneeprobe",
    meereshoehe = "H\u00f6he des Messpunkts \u00fcber Meer \\[m\\]",
    hanglage = "Ausrichtung des Hangs",
    kanton = "Kanton des Messpunkts",
    schneehoehe = paste(
      "Maximal gemessene Schneeh\u00f6he",
      "w\u00e4hrend der Messperiode [cm]"
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
  
  ## Rechtsschiefe H\u00f6henverteilung:
  ## ungef\u00e4hr Mittelwert 1200 m und Standardabweichung 358 m
  meereshoehe <- round(
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
      c("west/ost", "nord", "s\u00fcd"),
      size = n,
      replace = TRUE,
      prob = c(0.30, 0.40, 0.30)
    ),
    levels = c("west/ost", "nord", "s\u00fcd")
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
    "s\u00fcd" = -40
  )
  
  ## Kantonseffekte in Zentimetern
  kantonEffekt <- c(
    "BE" = 0,
    "GR" = 20,
    "TI" = -15,
    "UR" = 10,
    "VS" = -10
  )
  
  ## Erwartete maximale Schneeh\u00f6he
  erwartungswert <- 0.20 * meereshoehe +
    unname(hangEffekt[as.character(hanglage)]) +
    unname(kantonEffekt[as.character(kanton)])
  
  ## Beobachtete Schneeh\u00f6he mit zuf\u00e4lliger Streuung
  schneehoehe <- round(
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
    meereshoehe,
    hanglage,
    kanton,
    schneehoehe
  )
  
  return(res)
}

