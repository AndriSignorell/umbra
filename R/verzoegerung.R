
#' Lieferverzögerungen vor und nach einem Memo
#'
#' Lädt gepaarte Messungen zur Verzögerung der ersten Lieferung vor und nach
#' einer organisatorischen Intervention.
#'
#' @return ein gelabelter `data.frame` mit den gepaarten Verzögerungswerten.
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
verzoegerung <- function(){
  
  d.set <- .packageData("verzoegerung.xlsx")
  
  bedrock::label(d.set, TRUE) <- c("Messung vor dem Memo",     
                                   "Messung nach dem Memo" 
  )
  
  bedrock::label(d.set,) <- '
      In einem Speditionsbetrieb fiel dem Disponenten auf, dass die erste
      Lieferung h\u00e4ufig mit Versp\u00e4tung erfolgte.
      Wenn sich die erste Lieferung verz\u00f6gerte, verz\u00f6gerten sich auch
      alle anderen f\u00fcr diesen Tag geplanten Lieferungen. \u00dcber 15 Tage wurde in der Folge
      aufgezeichnet, wie viele Minuten nach der geplanten Zeit die erste
      Lieferung an jedem Tag vom Lager abging.
      <br>
      Danach wurde ein Memo an das gesamte Logistik-Personal versandt,
      in dem alle Beteiligte aufgefordert wurden,
      die Verz\u00f6gerung der Erstlieferung pro Tag zu verringern. Eine Woche nach dem Versand des Memos wurde
      erneut gepr\u00fcft, wie viele Minuten Verz\u00f6gerung die Erstlieferung
      aufwies.
  
      Die Daten finden sich in der Tabelle&nbsp;&nbsp;<strong>&link&</strong>&nbsp;&nbsp;. Eine negative Zahl
      in der Tabelle bedeutet dass die Lieferng fr\u00fcher als geplant vom Lager abging.
      Es interessiert nun, ob das Memo zu geringeren Versp\u00e4tungen gef\u00fchrt hat.'
  
}


