% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tell_me_how_drunk.R
\name{tell_me_how_drunk}
\alias{tell_me_how_drunk}
\title{Let's find out, how drunk are you?}
\usage{
tell_me_how_drunk(
  age,
  sex = c("male", "female"),
  height,
  weight,
  drinking_time,
  drinks
)
}
\arguments{
\item{age}{in years}

\item{sex}{male or female (character)}

\item{height}{in cm}

\item{weight}{in kg}

\item{drinking_time}{sorted POSIXct vector giving start and end of the party}

\item{drinks}{list or vector with names "massn", "hoibe", "wein", "schnaps"}
}
\value{
approximate BAC in per mille
}
\description{
computes approximate BAC (in per mille)
at the end of the party (\code{drinking_time[2]})
}
\details{
\href{https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/}{Method}
\itemize{
\item massn: 1l~6\%
\item hoibe: 0.5l~6\%
\item wein: 0.2l~11\%
\item schnaps: 0.04l~40\%
}
}
\examples{
tell_me_how_drunk(
  age = 39,
  sex = "male",
  height = 190,
  weight = 85,
  drinking_time = c(
    as.POSIXct("2021-01-28 14:00:00"),
    as.POSIXct("2021-01-28 16:00:00")
  ),
  drinks = c(schnaps = 1, hoibe = 3)
)
}
