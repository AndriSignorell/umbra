
# Binomial Library


LibBinomial <- list(
  zigaretten = list(
    PRE_TXT = "In einer Studie sollte die These geprüft werden, dass Raucher ihre
                  Zigarettenmarke aufgrund des Geschmacks zuverlässig identifizieren können. 
                  In einem Experiment wurden &n&&nbsp;Probanden je 5&nbsp;Zigaretten unterschiedlicher Marke 
                  zum Rauchen gegeben, von denen jeweils eine von der bevorzugten Marke war. X bezeichne die 
                  Anzahl Personen, die ihre Marke richtig erkannt haben.
                  <br>
                  Unter der Annahme, dass alle Probanden nur geraten haben, wie gross ist 
                  die Wahrscheinlichkeit, dass ...&nbsp;<br><br>
                  ",
    prob_txt = "Probanden ihre Marke richtig erkannt haben.",
    quant_txt = "Wie gross ist die höchste Anzahl richtig erkannter Zigarettenmarken, der %s TeilnehmerInnen 
                    mit den am wenigsten richtig erkannten Zigaretten?",  
    prob = c(1/5, 1/5),  # probability from ... to
    n = c(12,15)
  ), 
  
  
  fahrschule = list(
    PRE_TXT =  "Eine Fahrschule behauptet, dass &p& ihrer Fahrschüler beim ersten Versuch 
               die Fahrprüfung bestehen.  <br>
               Berechnen Sie die Wahrscheinlichkeit, dass bei dieser Erfolgsquote bei einer 
               Gruppe von &n&&nbsp;Fahrschülern ... <br><br>
              ",
    prob_txt="Fahrschüler die Prüfung bestehen.",
    quant_txt = "", 
    prob = c(0.7,0.9),  # probability from ... to
    n = c(6, 8)
    
  ), 
  
  sumpfkrebs = list(
    PRE_TXT =  "
        In der wissenschaftlichen Literatur findet sich die Schätzung, 
        dass unter normalen Umständen p&nbsp;=&nbsp;&p& der Sumpfkrebse 
        nicht fortpflanzungsfähig 
        sind. Ein Team von Biologie-Studenten fängt n&nbsp;=&nbsp;&n&&nbsp; erwachsene Krebse.
        <br>
               Berechnen Sie die Wahrscheinlichkeit, dass davon ... <br><br>
              ",
    prob_txt="Krebse nicht fortpflanzungsfähig sind.",
    quant_txt = "", 
    prob = c(0.1, 0.2),  # probability from ... to
    n = c(12, 15)
    
  ), 
  
  biathlon = list(
    PRE_TXT =  "
    
      Eine Biathletin startet mit einer Trefferwahrscheinlichkeit von p&nbsp;=&nbsp;&p& 
      in die Saison, bestimmt anhand ihrer Trainingsdaten. 
      In den ersten Rennen gibt sie n&nbsp;=&nbsp;&n&&nbsp; Schüsse ab.
      <br>
      Berechnen Sie die Wahrscheinlichkeit, dass davon ... <br><br>
      ",
    prob_txt="Schüsse Treffer sind.",
    quant_txt = "", 
    prob = c(0.85, 0.95),  # probability from ... to
    n = c(12, 15)
    
  ), 
  
  corner = list(
    PRE_TXT =  "
      Eine Fussballmannschaft schätzt, dass sie bei p&nbsp;=&nbsp;&p& der 
      Eckbälle ein Tor erzielt. In den verbleibenden 7 Spielen der Meisterschaft hofft 
      die Mannschaft insgesamt n&nbsp;=&nbsp;&n&&nbsp; Eckbälle zu treten. 
      <br><br>
      Wie gross ist die Wahrscheinlichkeit, dass die Mannschaft bei den 
      getretenen Eckbällen ... <br><br>
      ",
    prob_txt="Tore erzielt.",
    quant_txt = "", 
    prob = c(0.03, 0.07),  # probability from ... to
    n = c(12, 15)*7
    
  ),

  tennis = list(
    PRE_TXT = "\n
      Ein Tennisspieler schätzt, dass er bei jedem Aufschlag mit einer
      Wahrscheinlichkeit von p&nbsp;=&nbsp;&p& ein Ass erzielt.
      In den verbleibenden Partien des Turniers rechnet er mit insgesamt
      n&nbsp;=&nbsp;&n&&nbsp; Aufschlägen.
      <br><br>
      Wie gross ist die Wahrscheinlichkeit, dass der Tennisspieler bei diesen
      Aufschlägen ... <br><br>
    ",
    prob_txt="Asse erzielt.",
    quant_txt = "", 
    prob = c(0.08, 0.11),  # probability from ... to
    n = c(12, 15)*7
    
  )

  
  
)

