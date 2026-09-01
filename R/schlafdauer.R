
#' @export
schlafdauer <- function(n=156, nNa=0){
  
  n1_valid <- n; n1_na <- nNa; n1 <- n1_valid + n1_na
  
  grenzen     <- list(c(3,4), c(4,5), c(5,6), c(6,7), c(7,8), c(8,9), c(9,10))
  klassen_idx <- rep(seq_along(grenzen), 
                     round(sapply(grenzen, 
                                  function(x) diff(pnorm(x, mean=6.2, sd=1.3)))*n))
  schlaf_vals <- sapply(klassen_idx, function(k) {
    g <- grenzen[[k]]
    round(runif(1, g[1]+0.01, g[2]-0.01), 2)
  })
  
  schlafdauer   <- c(schlaf_vals, rep(NA, n1_na))[sample(n1)]
  alter1        <- round(runif(n1, 18, 72))
  geschlecht1   <- sample(c("weiblich","m\u00e4nnlich","divers"), n1, TRUE, c(.50,.47,.03))
  berufsgruppe1 <- sample(c("B\u00fcro","Handwerk","Gesundheit","Bildung","Andere"),
                          n1, TRUE, c(.35,.20,.18,.15,.12))
  sport_base    <- replace(schlafdauer, is.na(schlafdauer), 7)
  sport_h1      <- pmax(0, round(rnorm(n1, 8 - 0.4*(sport_base - 7), 2.5), 1))
  
  schlaf_df <- data.frame(schlafdauer, alter=alter1, geschlecht=geschlecht1,
                          berufsgruppe=berufsgruppe1, sport_h_woche=sport_h1)
  
  lbl <-  c(
    schlafdauer   = "Durchschnittliche Schlafdauer pro Nacht (in Stunden)",
    alter         = "Alter der befragten Person (in Jahren)",
    geschlecht    = "Geschlecht der befragten Person (weiblich / m\u00e4nnlich / divers)",
    berufsgruppe  = "Berufsfeld der befragten Person (B\u00fcro / Handwerk / Gesundheit / Bildung / Andere)",
    sport_h_woche = "W\u00f6chentlicher Zeitaufwand f\u00fcr Sport und Bewegung (in Stunden)"
  )
  
  bedrock::label(schlaf_df, vars = names(lbl)) <- lbl
  
  bedrock::label(schlaf_df) <- "Eine Schlafforscherin untersucht die Schlafdauer von 
  Erwachsenen in der Schweiz. Im Rahmen einer Studie wurde eine Stichprobe 
  zuf\u00e4llig ausgew\u00e4hlter Personen befragt. Erfasst wurde die durchschnittliche 
  Schlafdauer pro Nacht (in Stunden). 
  "
  schlaf_df
  
}

