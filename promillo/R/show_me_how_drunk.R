#' plot promille versus 5 minute time stamps of drinking time
#'
#' create line plot of promille versus 5 minute time stamps of drinking time
#'
#' @param age in years
#' @param sex male or female (character)
#' @param height in cm
#' @param weight in kg
#' @param drinking_time sorted POSIXct vector giving start and end of the party
#' @param drinks list or vector with names "massn", "hoibe", "wein", "schnaps"
#     counting the number consumed of each type of drink
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' show_me_how_drunk(
#'   age = 39,
#'   sex = "male",
#'   height = 190,
#'   weight = 85,
#'   drinking_time = c(
#'     as.POSIXct("2021-01-28 14:00:00"),
#'     as.POSIXct("2021-01-28 16:00:00")
#'   ),
#'   drinks = c(schnaps = 1, hoibe = 3)
#' )
show_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {
  # checking format of drinking time, other checks in function tell_me_how_drunk
  checkmate::assert_posixct(drinking_time,
    any.missing = FALSE, len = 2,
    sorted = TRUE
  )
  interval <- lubridate::interval(drinking_time[1], drinking_time[2])
  # count of 5 minute intervals between drinkingtime start and end
  steps_num <- interval / lubridate::dminutes(5)
  # timestamps corresponding to the 5 minute intervals
  steps <- c(drinking_time[1] + 60 * 5 * c(0:floor(steps_num)), drinking_time[2])
  promille_values <- vapply(steps, function(current_time) {
    tell_me_how_drunk(
      age, sex, height, weight, c(drinking_time[1], current_time),
      drinks
    )
  }, FUN.VALUE = numeric(1))
  ggplot2::qplot(
    x = steps, y = promille_values, geom = "line", xlab = "time",
    ylab = "promille"
  )
}
