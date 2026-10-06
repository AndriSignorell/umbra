
#' Löhne männlicher Arbeitnehmer im Mittelatlantik-Gebiet der USA
#'
#' Lohn und persönliche Merkmale von 2921 männlichen Arbeitnehmern aus der
#' Region Middle Atlantic der USA, erfasst in den Jahren 2003 bis 2009.
#'
#' @format Ein `data.frame` mit 2921 Zeilen und 10 Variablen. Jede Variable
#'   trägt ihre Beschreibung im Attribut `label`.
#' \describe{
#'   \item{year}{Jahr, in dem die Lohndaten erfasst wurden, 2003 bis 2009.}
#'   \item{age}{Alter des Arbeitnehmers in Jahren, 18 bis 80.}
#'   \item{maritl}{Zivilstand: `Never Married`, `Married`, `Widowed`,
#'     `Divorced`, `Separated`.}
#'   \item{race}{Ethnie: `White`, `Black`, `Asian`, `Other`.}
#'   \item{education}{Bildungsniveau, aufsteigend: `< HS Grad`, `HS Grad`,
#'     `Some College`, `College Grad`, `Advanced Degree`.}
#'   \item{region}{Region des Landes. Der Faktor hat die neun Regionen der
#'     USA als Stufen, es kommt aber nur `Middle Atlantic` vor.}
#'   \item{jobclass}{Art der Tätigkeit: `Industrial` oder `Information`.}
#'   \item{health}{Gesundheitszustand: `<=Good` oder `>=Very Good`.}
#'   \item{health_ins}{Ist der Arbeitnehmer krankenversichert? `Yes` oder
#'     `No`.}
#'   \item{wage}{Lohn, 20.1 bis 227.5. Die Quelle nennt keine Einheit; das
#'     Buch, zu dem die Daten gehören, liest die Werte als Jahreslohn in
#'     1000 US-Dollar.}
#' }
#'
#' @details
#' Der Datensatz ist `Wage` aus dem Paket \pkg{ISLR} (Version 1.4), mit vier
#' Änderungen:
#'
#' * Die 79 Arbeitnehmer mit einem Lohn über 250 fehlen. Sie bilden im
#'   Original eine abgesetzte Gruppe von Spitzenverdienern; zwischen 227.5
#'   und 256.4 liegt dort kein einziger Wert.
#' * Die Variable `logwage`, der Logarithmus des Lohns, ist weggelassen.
#' * Die Stufen der Faktoren stehen ohne die vorangestellte Nummer, also
#'   `Married` statt `2. Married`.
#' * Die Variablen sind deutsch beschriftet.
#'
#' Alle übrigen Werte stimmen mit dem Original überein.
#'
#' Geeignete Verfahren sind Skalenbestimmung, Klasseneinteilungen,
#' empirische Verteilungen, Kennzahlen, Gruppenvergleiche, Varianzanalyse
#' sowie einfache und multiple lineare Regression mit Dummy-Variablen.
#'
#' @source Paket \pkg{ISLR}, Datensatz `Wage`. Dort: von Steve Miller,
#'   Inquidia Consulting (vormals Open BI), von Hand zusammengestellt aus dem
#'   March 2011 Supplement des Current Population Survey.
#'
#' @references
#' James, G., Witten, D., Hastie, T. and Tibshirani, R. (2013)
#' *An Introduction to Statistical Learning with Applications in R*.
#' Springer, New York. <https://www.statlearning.com>
#'
#' @examples
#' str(d.wage)
#'
#' # die Beschriftungen der Variablen
#' sapply(d.wage, attr, "label")
#'
#' # Lohn nach Bildungsniveau
#' aggregate(wage ~ education, data = d.wage, FUN = median)
#' boxplot(wage ~ education, data = d.wage)
#'
#' summary(lm(wage ~ age + education + jobclass, data = d.wage))
#'
#' @concept 1.2 Daten und Skalen
#' @concept 1.3 Empirische Verteilungen
#' @concept 1.4 Kennzahlen
#' @concept 1.5 Bivariate Datenanalyse
#' @concept 1.16 Varianzanalyse
#' @concept 1.19 Multiple lineare Regression
#' @keywords datasets
"d.wage"
