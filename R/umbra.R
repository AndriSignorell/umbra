

homeoffice <- function() {  # 
  
set.seed(123)

n <- 250

# -----------------------------
# Kategoriale Variablen
# -----------------------------

# Geschlecht
geschlecht <- sample(
  c("weiblich", "männlich"),
  n,
  replace = TRUE,
  prob = c(0.48, 0.52)
)

# Abteilung
abteilung <- sample(
  c("HR", "IT", "Finanzen", "Marketing", "Administration"),
  n,
  replace = TRUE,
  prob = c(0.15, 0.30, 0.20, 0.15, 0.20)
)

# Stellung im Unternehmen
stellung <- sample(
  c("Mitarbeiter", "Teamleitung", "Management"),
  n,
  replace = TRUE,
  prob = c(0.72, 0.20, 0.08)
)

# Beschäftigungsgrad
pensum <- sample(
  c("80%", "90%", "100%"),
  n,
  replace = TRUE,
  prob = c(0.20, 0.15, 0.65)
)

# -----------------------------
# Metrische Variablen
# -----------------------------

# Alter
alter <- round(rnorm(n, mean = 39, sd = 10))
alter[alter < 18] <- 18

# Jahre im Betrieb
jahre_betrieb <- round(rgamma(n, shape = 2.5, scale = 3), 1)

# Arbeitszufriedenheit (0-100)
zufriedenheit <- round(rnorm(n, mean = 72, sd = 12))
zufriedenheit[zufriedenheit < 0] <- 0
zufriedenheit[zufriedenheit > 100] <- 100

# -----------------------------
# Homeoffice-Tage
# abhängig von Abteilung,
# Stellung und Alter
# -----------------------------

homeoffice_tage <- numeric(n)

for(i in 1:n){
  
  basis <- 8
  
  # IT mehr Homeoffice
  if(abteilung[i] == "IT"){
    basis <- basis + 4
  }
  
  # Administration weniger
  if(abteilung[i] == "Administration"){
    basis <- basis - 2
  }
  
  # Führungskräfte eher weniger
  if(stellung[i] == "Management"){
    basis <- basis - 3
  }
  
  if(stellung[i] == "Teamleitung"){
    basis <- basis - 1
  }
  
  # Jüngere etwas häufiger Homeoffice
  basis <- basis - 0.05 * (alter[i] - 40)
  
  # Zufall
  homeoffice_tage[i] <- round(
    rnorm(1, mean = basis, sd = 3)
  )
}

# Grenzen setzen
homeoffice_tage[homeoffice_tage < 0] <- 0
homeoffice_tage[homeoffice_tage > 22] <- 22

# Fehlende Werte
homeoffice_tage[sample(1:n, 12)] <- NA

# -----------------------------
# Datensatz
# -----------------------------

daten <- data.frame(
  geschlecht,
  abteilung,
  stellung,
  pensum,
  alter,
  jahre_betrieb,
  zufriedenheit,
  homeoffice_tage
)

bedrock::label(daten) <- "Ein grosses Schweizer Unternehmen möchte untersuchen, 
wie stark Mitarbeitende das Homeoffice-Angebot tatsächlich nutzen. 
Dazu wurden zufällig 250 Mitarbeitende aus verschiedenen Abteilungen 
ausgewählt. Erfasst wurde die Anzahl Homeoffice-Tage pro Monat. 
Die Unternehmensleitung interessiert sich insbesondere für die 
durchschnittliche Nutzung sowie für mögliche starke Unterschiede 
zwischen Mitarbeitenden."

daten

}



amt <- function() {
  
set.seed(321)

n <- 300

# --------------------------------
# Kategoriale Variablen
# --------------------------------

# Abteilung
abteilung <- sample(
  c("Bauamt", "Steueramt", "Einwohneramt"),
  n,
  replace = TRUE,
  prob = c(0.35, 0.35, 0.30)
)

# Priorität des Gesuchs
prioritaet <- sample(
  c("niedrig", "mittel", "hoch"),
  n,
  replace = TRUE,
  prob = c(0.40, 0.45, 0.15)
)

# Digitalisierung
digital <- sample(
  c("digital", "papier"),
  n,
  replace = TRUE,
  prob = c(0.70, 0.30)
)

# Sachbearbeiter-Stufe
sachbearbeiter <- sample(
  c("Junior", "Senior", "Expert"),
  n,
  replace = TRUE,
  prob = c(0.40, 0.45, 0.15)
)

# --------------------------------
# Metrische Variablen
# --------------------------------

# Alter der Sachbearbeitenden
alter <- round(rnorm(n, mean = 41, sd = 11))
alter[alter < 20] <- 20

# Jahre Berufserfahrung
berufserfahrung <- round(rgamma(n, shape = 2.5, scale = 4), 1)

# Anzahl paralleler Dossiers
anzahl_dossiers <- round(rnorm(n, mean = 24, sd = 8))
anzahl_dossiers[anzahl_dossiers < 1] <- 1

# --------------------------------
# Bearbeitungszeit simulieren
# --------------------------------

bearbeitungszeit <- numeric(n)

for(i in 1:n){
  
  basis <- 15
  
  # Abteilungseffekte
  if(abteilung[i] == "Bauamt"){
    basis <- basis + 10
  }
  
  if(abteilung[i] == "Steueramt"){
    basis <- basis + 5
  }
  
  if(abteilung[i] == "Einwohneramt"){
    basis <- basis - 2
  }
  
  # Priorität
  if(prioritaet[i] == "hoch"){
    basis <- basis - 5
  }
  
  if(prioritaet[i] == "niedrig"){
    basis <- basis + 3
  }
  
  # Digitalisierung
  if(digital[i] == "digital"){
    basis <- basis - 4
  }
  
  # Erfahrung reduziert Zeit
  basis <- basis - 0.15 * berufserfahrung[i]
  
  # Viele Dossiers erhöhen Zeit
  basis <- basis + 0.25 * anzahl_dossiers[i]
  
  # Sachbearbeiter-Level
  if(sachbearbeiter[i] == "Expert"){
    basis <- basis - 3
  }
  
  if(sachbearbeiter[i] == "Junior"){
    basis <- basis + 2
  }
  
  # Zufall
  bearbeitungszeit[i] <- round(
    rnorm(1, mean = basis, sd = 5),
    1
  )
}

# Mindestwert
bearbeitungszeit[bearbeitungszeit < 1] <- 1

# Fehlende Werte
bearbeitungszeit[sample(1:n, 10)] <- NA

# --------------------------------
# Datensatz
# --------------------------------

daten <- data.frame(
  abteilung,
  prioritaet,
  digital,
  sachbearbeiter,
  alter,
  berufserfahrung,
  anzahl_dossiers,
  bearbeitungszeit
)

bedrock::label(daten) <- "Eine kantonale Verwaltung untersucht die Effizienz 
verschiedener Abteilungen. Dabei interessiert insbesondere, 
ob sich die Bearbeitungsdauer von Gesuchen zwischen den 
Abteilungen unterscheidet. Untersucht werden die Bereiche 
Bauamt, Steueramt und Einwohneramt.
<br>
Die Bearbeitungszeit wird in Tagen gemessen."

daten
}


leistung <- function(){
  
  set.seed(555)
  
  n <- 180
  
  # --------------------------------
  # Kategoriale Variablen
  # --------------------------------
  
  # Sportart
  sportart <- sample(
    c("Ausdauer", "Teamsport", "Kraftsport"),
    n,
    replace = TRUE,
    prob = c(0.40, 0.35, 0.25)
  )
  
  # Geschlecht
  geschlecht <- sample(
    c("weiblich", "männlich"),
    n,
    replace = TRUE
  )
  
  # Trainingsniveau
  niveau <- sample(
    c("Amateur", "Fortgeschritten", "Elite"),
    n,
    replace = TRUE,
    prob = c(0.45, 0.40, 0.15)
  )
  
  # Rauchstatus
  raucher <- sample(
    c("ja", "nein"),
    n,
    replace = TRUE,
    prob = c(0.15, 0.85)
  )
  
  # --------------------------------
  # Metrische Variablen
  # --------------------------------
  
  # Alter
  alter <- round(rnorm(n, mean = 26, sd = 6))
  alter[alter < 16] <- 16
  
  # Trainingsstunden pro Woche
  trainingsstunden <- round(rnorm(n, mean = 8, sd = 3), 1)
  trainingsstunden[trainingsstunden < 1] <- 1
  
  # Ruhepuls
  ruhepuls <- round(rnorm(n, mean = 62, sd = 8))
  ruhepuls[ruhepuls < 40] <- 40
  
  # Körperfettanteil
  koerperfett <- round(rnorm(n, mean = 18, sd = 6), 1)
  koerperfett[koerperfett < 4] <- 4
  
  # --------------------------------
  # VO2max simulieren
  # --------------------------------
  
  vo2max <- numeric(n)
  
  for(i in 1:n){
    
    basis <- 45
    
    # Sportart
    if(sportart[i] == "Ausdauer"){
      basis <- basis + 10
    }
    
    if(sportart[i] == "Kraftsport"){
      basis <- basis - 5
    }
    
    # Trainingsniveau
    if(niveau[i] == "Fortgeschritten"){
      basis <- basis + 5
    }
    
    if(niveau[i] == "Elite"){
      basis <- basis + 12
    }
    
    # Geschlecht
    if(geschlecht[i] == "männlich"){
      basis <- basis + 4
    }
    
    # Rauchen verschlechtert VO2max
    if(raucher[i] == "ja"){
      basis <- basis - 6
    }
    
    # Mehr Training verbessert VO2max
    basis <- basis + 0.8 * trainingsstunden[i]
    
    # Höherer Ruhepuls eher schlechter
    basis <- basis - 0.15 * (ruhepuls[i] - 60)
    
    # Mehr Körperfett eher schlechter
    basis <- basis - 0.4 * (koerperfett[i] - 18)
    
    # Zufall
    vo2max[i] <- round(
      rnorm(1, mean = basis, sd = 5),
      1
    )
  }
  
  # Grenzen setzen
  vo2max[vo2max < 20] <- 20
  vo2max[vo2max > 75] <- 75
  
  # Fehlende Werte
  vo2max[sample(1:n, 6)] <- NA
  
  # --------------------------------
  # Datensatz
  # --------------------------------
  
  daten <- data.frame(
    sportart,
    geschlecht,
    niveau,
    raucher,
    alter,
    trainingsstunden,
    ruhepuls,
    koerperfett,
    vo2max
  )
  
  bedrock::label(daten) <- "Ein Leistungszentrum untersucht die 
  aerobe Leistungsfähigkeit seiner 
  Athletinnen und Athleten. Dazu wurde bei einer Stichprobe von Sportlern 
  die maximale Sauerstoffaufnahme (VO₂max) gemessen.

  Die Werte sollen anschliessend in Klassen eingeteilt werden,
  um die Verteilung der Leistungsfähigkeit besser beurteilen zu können."
  daten
  
}


schlaf <- function(){
  set.seed(777)
  
  n <- 220
  
  # --------------------------------
  # Kategoriale Variablen
  # --------------------------------
  
  # Geschlecht
  geschlecht <- sample(
    c("weiblich", "männlich"),
    n,
    replace = TRUE,
    prob = c(0.60, 0.40)
  )
  
  # Studienjahr
  studienjahr <- sample(
    c("1. Jahr", "2. Jahr", "3. Jahr", "4. Jahr", "5. Jahr"),
    n,
    replace = TRUE,
    prob = c(0.22, 0.22, 0.20, 0.18, 0.18)
  )
  
  # Sportliche Aktivität
  sport <- sample(
    c("wenig", "moderat", "viel"),
    n,
    replace = TRUE,
    prob = c(0.30, 0.50, 0.20)
  )
  
  # Nebenjob
  nebenjob <- sample(
    c("ja", "nein"),
    n,
    replace = TRUE,
    prob = c(0.45, 0.55)
  )
  
  # --------------------------------
  # Metrische Variablen
  # --------------------------------
  
  # Alter
  alter <- round(rnorm(n, mean = 24, sd = 3))
  alter[alter < 18] <- 18
  
  # Lernstunden pro Woche
  lernstunden <- round(rnorm(n, mean = 28, sd = 10), 1)
  lernstunden[lernstunden < 5] <- 5
  
  # Koffeinkonsum pro Tag
  koffein <- round(rnorm(n, mean = 2.5, sd = 1.5), 1)
  koffein[koffein < 0] <- 0
  
  # Stresslevel (0-100)
  stresslevel <- round(rnorm(n, mean = 65, sd = 15))
  stresslevel[stresslevel < 0] <- 0
  stresslevel[stresslevel > 100] <- 100
  
  # --------------------------------
  # Schlafdauer simulieren
  # --------------------------------
  
  schlafdauer <- numeric(n)
  
  for(i in 1:n){
    
    basis <- 7.5
    
    # Höhere Studienjahre schlafen etwas weniger
    if(studienjahr[i] == "4. Jahr"){
      basis <- basis - 0.4
    }
    
    if(studienjahr[i] == "5. Jahr"){
      basis <- basis - 0.7
    }
    
    # Viel Lernen reduziert Schlaf
    basis <- basis - 0.03 * lernstunden[i]
    
    # Mehr Stress reduziert Schlaf
    basis <- basis - 0.015 * stresslevel[i]
    
    # Sport verbessert Schlaf leicht
    if(sport[i] == "viel"){
      basis <- basis + 0.5
    }
    
    # Nebenjob reduziert Schlaf leicht
    if(nebenjob[i] == "ja"){
      basis <- basis - 0.3
    }
    
    # Viel Koffein eher weniger Schlaf
    basis <- basis - 0.08 * koffein[i]
    
    # Zufall
    schlafdauer[i] <- round(
      rnorm(1, mean = basis, sd = 0.8),
      1
    )
  }
  
  # Grenzen setzen
  schlafdauer[schlafdauer < 3.5] <- 3.5
  schlafdauer[schlafdauer > 10] <- 10
  
  # Fehlende Werte
  schlafdauer[sample(1:n, 8)] <- NA
  
  # --------------------------------
  # Datensatz
  # --------------------------------
  
  daten <- data.frame(
    geschlecht,
    studienjahr,
    sport,
    nebenjob,
    alter,
    lernstunden,
    koffein,
    stresslevel,
    schlafdauer
  )
  
  bedrock::label(daten) <- "Ein universitäres Forschungsprojekt untersucht 
  das Schlafverhalten von Medizinstudierenden während des Semesters. 
  Besonders interessiert die Forschenden, ob Faktoren wie Lernaufwand, 
  Studienjahr oder körperliche Aktivität mit der täglichen Schlafdauer 
  zusammenhängen. Dazu wurden Studierende verschiedener Jahrgänge zu 
  ihrem Alltag und ihren Schlafgewohnheiten befragt.<br>
  Die tägliche Schlafdauer wird in Stunden gemessen."
  
  daten
  
  
}


