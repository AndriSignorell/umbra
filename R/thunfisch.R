
## Fiktive Lehrdaten:
## Atlantischer Blauflossenthunfisch in norwegischen Gewässern
thunfisch <- function(seed) {

  set.seed(seed)
  
  n <- 360L
  
  ## Untersuchungszeitraum: je 180 Fische
  zeitraum <- factor(
    rep(c("2016–2018", "2023–2025"), each = n / 2L),
    levels = c("2016–2018", "2023–2025")
  )
  
  ## Drei norwegische Küstenregionen
  region <- factor(
    sample(
      c("Rogaland", "Vestland", "Møre og Romsdal"),
      size = n,
      replace = TRUE,
      prob = c(0.35, 0.40, 0.25)
    ),
    levels = c("Rogaland", "Vestland", "Møre og Romsdal")
  )
  
  ## Geschlecht
  geschlecht <- factor(
    sample(
      c("weiblich", "männlich"),
      size = n,
      replace = TRUE
    ),
    levels = c("weiblich", "männlich")
  )
  
  ## In der aktuellen Periode kommen insbesondere in Rogaland und
  ## Vestland weniger ältere Tiere in den Fängen vor.
  rueckgangAlter <- ifelse(
    zeitraum == "2023–2025",
    unname(
      c(
        "Rogaland" = 1.8,
        "Vestland" = 1.2,
        "Møre og Romsdal" = 0.2
      )[as.character(region)]
    ),
    0
  )
  
  alterJahre <- round(
    rnorm(
      n,
      mean = 11.5 - rueckgangAlter,
      sd = 1.8
    )
  )
  
  alterJahre <- pmin(18, pmax(6, alterJahre))
  
  ## Relativer Ernährungs- bzw. Körperzustand:
  ## Werte um 1 entsprechen einem durchschnittlichen Zustand.
  konditionsindex <- rnorm(
    n,
    mean = 1.00 - 0.018 * (zeitraum == "2023–2025"),
    sd = 0.055
  )
  
  konditionsindex <- pmin(
    1.18,
    pmax(0.82, konditionsindex)
  )
  
  ## Körperlänge
  laengeCm <- round(
    165 +
      7.6 * alterJahre +
      3 * (geschlecht == "weiblich") +
      rnorm(n, mean = 0, sd = 7),
    digits = 1
  )
  
  ## Kleine regionale Unterschiede bei gleichem Alter und gleicher Länge
  regionEffekt <- unname(
    c(
      "Rogaland" = -5,
      "Vestland" = 0,
      "Møre og Romsdal" = 6
    )[as.character(region)]
  )
  
  ## Körpergewicht
  gewichtKg <- round(
    -450 +
      2.45 * laengeCm +
      7.0 * alterJahre +
      90 * (konditionsindex - 1) +
      regionEffekt +
      rnorm(n, mean = 0, sd = 15),
    digits = 1
  )
  
  gewichtKg <- pmax(gewichtKg, 100)
  
  ## Für die Untersuchung definierte Gewichtsklasse.
  ## 200 kg sind hier eine betriebliche Beobachtungsschwelle,
  ## keine gesetzliche Fanggrenze.
  unter200Kg <- factor(
    ifelse(gewichtKg < 200, "ja", "nein"),
    levels = c("nein", "ja")
  )
  
  ## Endgültiger Datensatz mit acht Variablen
  res <- data.frame(
    zeitraum,
    region,
    geschlecht,
    alterJahre,
    laengeCm,
    konditionsindex = round(konditionsindex, 3),
    gewichtKg,
    unter200Kg
  )

  label(res, vars=TRUE) <- c(
    "Referenzperiode oder aktuelle Periode"
    , "norwegische Fangregion"
    , "Geschlecht des Fisches"
    , "geschätztes Alter in Jahren"
    , "Körperlänge in Zentimetern"
    , "relativer Körperzustand; Werte um 1 sind durchschnittlich"
    , "Gewicht in Kilogramm"
    , "Gewicht unter 200 kg: ja oder nein"
  )

  # story
  label(res) <- "Das norwegische Fiskeridirektorat beauftragt das 
    Havforskningsinstituttet, Fänge aus drei Regionen zu untersuchen. 
    Verglichen werden eine Referenzperiode 2016–2018 und eine aktuelle Periode 2023–2025. 
    Der simulierte Rückgang fällt in Rogaland und Vestland stärker 
    aus als in Møre og Romsdal."   

  res
  
}

