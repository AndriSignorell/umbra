
#' Operationsverspätungen vor und nach einem Memo
#'
#' Liefert gepaarte Messungen zur Verspätung der ersten geplanten Operation vor
#' und nach einer organisatorischen Intervention.
#'
#' @return ein gelabelter `data.frame` mit den gepaarten Verspätungswerten.
#'
#' @details Geeignete Verfahren sind Differenzen, Konfidenzintervalle,
#' gepaarter t-Test und nichtparametrische Tests für verbundene Stichproben.
#'
#' @concept 1.4 Kennzahlen
#' @concept 1.12 Stichproben und Konfidenzintervalle
#' @concept 1.13 Grundlagen des Signifikanztests
#' @concept 1.14 t-Tests
#' @concept 1.17 Nichtparametrische Tests
#' @export
operation <- function(){
  
  d.set <- .packageData("operation.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Verspätung vor dem Memo",     
                                   "Verspätung nach Versenden des Memo"
  )
  
  bedrock::label(d.set,) <- "Der administrativen Leitung eines Spitals war aufgefallen, dass die erste nicht 
      notfallmässige Operation, die täglich angesetzt war, häufig mit Verspätung begann. 
      Wenn sich indes der erste geplante Eingriff verzögerte, verzögerten sich auch 
      alle anderen für diesen Tag geplanten Eingriffe. Über &n& Tage wurde in der Folge
      aufgezeichnet, wie viele Minuten nach der geplanten Zeit die erste
      Operation an jedem Tag begann. 
      <br>Danach wurde ein Memo an das gesamte chirurgische Personal des 
      Krankenhauses versandt, in dem alle Beteiligte aufgefordert wurden, 
      die Verzögerung des Beginns der ersten nicht dringenden Operation 
      pro Tag zu verringern. Eine Woche nach dem Versand des Memos wurde wiederum an &n& Tagen
      geprüft, wie viele Minuten Verzögerung die erste geplante Operation 
      aufwies."
  
  return(d.set)
  
}


