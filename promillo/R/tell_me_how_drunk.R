#' Let's find out, how drunk are you?
#'
#' computes approximate BAC (in per mille)
#' at the end of the party (`drinking_time[2]`)
#'
#'
#' [Method](https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/)
#'
#' - massn: 1l~6%
#' - hoibe: 0.5l~6%
#' - wein: 0.2l~11%
#' - schnaps: 0.04l~40%
#'
#' @param age in years
#' @param sex male or female (character)
#' @param height in cm
#' @param weight in kg
#' @param drinking_time sorted POSIXct vector giving start and end of the party
#' @param drinks list or vector with names "massn", "hoibe", "wein", "schnaps"
#     counting the number consumed of each type of drink
#'
#' @return approximate BAC in per mille
#' @export
#'
#' @examples
#' tell_me_how_drunk(
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
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

# utilities --------------------------------------------------------------------


#' compute consumed alcohol in g
#'
#' @param drinks list or vector with names "massn", "hoibe", "wein", "schnaps"
#     counting the number consumed of each type of drink
#'
#' @return consumed alcohol in g
#'
get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  checkmate::assert_subset(names(drinks),
    choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE
  )
  checkmate::assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40
  )
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4
  )
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
    alcohol_concentration[names(drinks)] * alcohol_density)
}


#' compute amount of water in a human body
#'
#' @param age in years
#' @param sex male or female (character)
#' @param height in cm
#' @param weight in kg
#'
#' @return amount of water in a human body
#'
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  checkmate::assert_number(age, lower = 10, upper = 110)
  if (age < 16 | age > 90) {
    warning("illegal")
  }
  checkmate::assert_number(height, lower = 100, upper = 230)
  checkmate::assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}


#' compute BAC at `drinking_time[2]`
#'
#' @param alcohol_drunk consumed alcohol in g
#' @param bodywater amount of water in a human body
#' @param drinking_time sorted POSIXct vector giving start and end of the party
#'
#' @return BAC at `drinking_time[2]`
#'
get_permille <- function(alcohol_drunk, bodywater, drinking_time) {
  checkmate::assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille - (max(0, partylength - 1) * sober_per_hour))
}
