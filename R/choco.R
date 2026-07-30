


choco <- function(n){
  
  n <- 300  
  dat <- data.frame(id=sample(1000:9999, n), 
                      geschlecht=sample(c("m","w"), n, r=T, prob=c(0.37, .73)),
                      alter = sample(18:75, n, r=TRUE),
                      region= region <- sample(c("de","fr","it"), n, r=T, 
                                               prob=c(0.14, 0.24, .62)),
                      menge=menge <- rpois(n, lambda = c("de"=1.8,"it"=3,"fr"=2.2)[region]),
                      mengekat= cut(menge, breaks=c(0,1,3,5, Inf), right=FALSE, 
                                    labels = c("0", "1-2", "3-4", "5.."))
  )
  bedrock::label(dat, vars=TRUE) <- c("ID", "Geschlecht", "Alter", 
                                        "Wohnregion", "Bezogene Packungen Schokolade", 
                                        "Mengen-Kategorie")
  
  PRE_TXT <- '
          Ein Schweizer Detailhändler will herausfinden, ob die Absatzmenge 
          einer bestimmten Schokoladensorte 
          durch die regional unterschiedlichen ökonomischen, sozialen und 
          kulturellen Gegebenheiten beeinflusst wird. Dazu wurden Personen 
          in verschiedenen Sprachregionen der Schweiz befragt, wie viele 
          Packungen sie pro Monat kaufen. 
          Die entsprechende Variable <em>mengekat</em> hat vier 
          Ausprägungen <em>"0", "1-2", "3-4"</em> und <em>"5.."</em> (letzere 
          5 oder mehr Packungen). <br>
          Es stellt sich die Frage nach dem Zusammenhang 
          zwischen Region und Kaufmenge?<br><br>'

    attr(dat, "desc") <- PRE_TXT
  
    dat
}


