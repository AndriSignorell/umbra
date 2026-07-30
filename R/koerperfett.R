
#' Body fat before and after a year of training
#'
#' Body fat percentage of gym members, measured by their trainer before and
#' after a year of training three times a week, together with the covariates
#' needed for a regression exercise. Two members did not show up for the
#' second measurement.
#'
#' The values are synthetic. They were generated from a stated model, so
#' that a solution can be checked against what the data were built to
#' contain:
#'
#' \itemize{
#'   \item body fat before training rises with age (about 0.1 points per
#'     year) and with BMI (about 0.55 points per BMI unit), falls with
#'     weekly training hours, and starts about 6 points higher for women
#'   \item the reduction over the year grows with the weekly training
#'     hours (about 0.25 points per hour) and is slightly larger for women
#'   \item measurement noise is of the order of 1.5 points, which is
#'     realistic for this kind of measurement and keeps the change from
#'     being a deterministic function of the training hours
#' }
#'
#' The numbers are stored literally rather than drawn at call time: an exam
#' variant must show the same data today and in three semesters.
#'
#' @format A data frame with 48 rows and 7 variables:
#' \describe{
#'   \item{geschlecht}{factor, `"m"` or `"w"`}
#'   \item{alter}{age in years}
#'   \item{groesse}{height in cm}
#'   \item{gewicht}{weight in kg}
#'   \item{training}{hours of training per week}
#'   \item{fettVorher}{body fat in percent, before the training year}
#'   \item{fettNachher}{body fat in percent, after it; 2 values missing}
#' }
#'
#' @return the data frame, carrying the exercise text in the attribute
#'   `label`
#'
#' @examples
#' d <- koerperfett()
#'
#' # paired comparison
#' t.test(d$fettNachher, d$fettVorher, paired = TRUE, alternative = "less")
#'
#' # regression, with BMI derived from height and weight
#' d$bmi <- d$gewicht / (d$groesse / 100)^2
#' summary(lm(fettNachher ~ alter + geschlecht + training + bmi, data = d))
#'
#' @family datasets
#' @concept teaching
#'
#' @export
koerperfett <- function() {

  res <- data.frame(

    geschlecht = factor(rep(c("m", "w"), 24), levels = c("m", "w")),

    alter = c(
      29, 56, 44, 20, 29, 28, 22, 60, 42, 60, 38, 43, 47, 35, 53, 42,
      37, 30, 50, 38, 47, 49, 33, 38, 47, 30, 41, 30, 62, 32, 22, 61,
      33, 37, 47, 61, 50, 38, 20, 22, 38, 57, 52, 50, 59, 62, 43, 20),

    groesse = c(
      186, 161, 177, 176, 181, 168, 185, 179, 173, 165, 173, 168, 189,
      180, 175, 176, 182, 159, 177, 167, 183, 166, 181, 168, 180, 163,
      172, 166, 173, 171, 174, 166, 172, 157, 170, 165, 190, 157, 175,
      178, 184, 177, 173, 165, 185, 165, 177, 165),

    gewicht = c(
      82.8, 49.3, 53.6, 60.2, 80.5, 56.7, 79.5, 65.5, 83.4, 61.9, 71.5,
      80.2, 97.1, 70.1, 78.1, 63.2, 87.7, 62.7, 72.6, 79.2, 78.6, 72.0,
      76.8, 70.2, 88.8, 56.9, 69.7, 64.3, 87.1, 74.7, 55.1, 60.6, 62.9,
      50.5, 70.1, 56.0, 93.5, 56.9, 66.5, 68.9, 63.4, 75.7, 77.1, 64.7,
      83.9, 50.4, 66.4, 69.3),

    training = c(
      4.6, 3.6, 3.9, 2.9, 2.1, 4.0, 3.6, 3.9, 3.5, 2.5, 4.3, 3.0, 3.4,
      1.6, 3.8, 3.1, 3.0, 4.8, 4.0, 4.2, 2.2, 2.1, 2.7, 5.7, 3.7, 2.6,
      2.2, 3.0, 3.1, 6.2, 2.0, 4.1, 3.8, 1.5, 1.6, 2.1, 5.0, 3.1, 2.2,
      4.5, 3.8, 4.8, 3.2, 4.0, 3.2, 3.8, 2.9, 2.1),

    fettVorher = c(
      11.0, 20.5, 12.7, 18.3, 15.0, 16.0, 14.6, 23.2, 18.2, 20.8, 15.3,
      24.6, 16.2, 23.8, 18.6, 21.9, 16.6, 17.6, 14.5, 22.5, 15.8, 25.6,
      14.3, 24.3, 17.4, 20.0, 16.2, 19.5, 16.6, 22.7, 10.3, 21.5, 12.4,
      18.5, 15.0, 21.6, 16.5, 22.8, 14.5, 16.9, 14.4, 25.2, 15.6, 18.5,
      17.9, 21.7, 12.5, 18.2),

    fettNachher = c(
      11.8, 23.3, 8.2, 16.0, 15.3, 15.9, 11.1, 19.6, 19.1, 23.9, 11.2,
      NA, 14.0, 22.3, 18.3, 21.4, 18.0, 12.4, 14.5, 19.4, 12.3, 24.4,
      13.9, 22.4, 16.4, 16.6, 14.6, 17.2, 13.8, 18.7, 7.5, 21.7, 12.4,
      18.5, NA, 18.7, 16.6, 22.0, 11.6, 17.3, 13.6, 24.2, 14.3, 17.0,
      14.4, 22.3, 9.5, 17.2))

  attr(res, "label") <- paste(
    "Eine Methode zur Messung der Fitness einer Person ist die Messung",
    "ihres K\u00f6rperfettanteils. Durchschnittliche K\u00f6rperfettanteile",
    "variieren je nach Alter, doch laut einigen Richtlinien liegt der",
    "Normalbereich f\u00fcr M\u00e4nner bei 15\u201320&nbsp;% K\u00f6rperfett",
    "und f\u00fcr Frauen bei 20\u201325&nbsp;% K\u00f6rperfett.",
    "Die Stichprobendaten stammen von einer Gruppe von M\u00e4nnern und",
    "Frauen, die w\u00e4hrend einem Jahr dreimal pro Woche in einem",
    "Fitnessstudio trainiert haben. Ihr K\u00f6rperfettanteil wurde zu",
    "Beginn und am Ende des Jahres gemessen.")

  res
}
