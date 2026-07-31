


.assertSampleSize <- function(n, max = Inf) {
  
  if (length(n) != 1L ||
      is.na(n) ||
      !is.numeric(n) ||
      n < 1L ||
      n != as.integer(n) ||
      n > max) {
    
    stop(
      sprintf(
        "'n' muss eine ganze Zahl zwischen 1 und %s sein.",
        format(max)
      ),
      call. = FALSE
    )
  }
  
  as.integer(n)
}




.labelEmulData <- function(x, meta) {
  
  labels <- meta$variables
  
  if (!identical(names(x), names(labels))) {
    
    missingLabels <- setdiff(names(x), names(labels))
    unknownLabels <- setdiff(names(labels), names(x))
    
    msg <- c(
      if (length(missingLabels)) {
        paste(
          "fehlende Labels:",
          paste(missingLabels, collapse = ", ")
        )
      },
      if (length(unknownLabels)) {
        paste(
          "unbekannte Labels:",
          paste(unknownLabels, collapse = ", ")
        )
      }
    )
    
    stop(
      paste(msg, collapse = "; "),
      call. = FALSE
    )
  }
  
  bedrock::label(x, vars = TRUE) <- unname(labels)
  bedrock::label(x) <- meta$description
  
  return(x)
}



.rdVariables <- function(x) {
  
  descriptions <- unname(x)
  
  descriptions <- ifelse(
    grepl("[.!?]$", descriptions),
    descriptions,
    paste0(descriptions, ".")
  )
  
  paste(
    paste0(
      "* `",
      names(x),
      "`: ",
      descriptions
    ),
    collapse = "\n"
  )
}



.emulConcepts <- c(
  "Daten und Skalen",
  "Empirische Verteilungen",
  "Kennzahlen",
  "Bivariate Datenanalyse",
  "Konzentration",
  "Grundlagen Wahrscheinlichkeit",
  "Rechnen mit Wahrscheinlichkeiten",
  "Zufallsvariablen und Verteilungen",
  "Diskrete Verteilungen",
  "Kontinuierliche Verteilungen",
  "Stichproben und Konfidenzintervalle",
  "Grundlagen des Signifikanztests",
  "t-Tests",
  "Chi-Quadrat-Tests",
  "Varianzanalyse",
  "Nichtparametrische Tests",
  "Einfache lineare Regression",
  "Multiple lineare Regression"
)


