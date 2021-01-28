test_that("basic functionality", {
  expect_true(
    ggplot2::is.ggplot(
      show_me_how_drunk(
        age = 68,
        sex = "male",
        height = 169,
        weight = 84,
        drinking_time = as.POSIXct(c("2016-10-03 08:10:00",
                                     "2016-10-03 08:15:00")),
        drinks = c("schnaps" = 3)
      )
    )
  )
  expect_true(
    ggplot2::is.ggplot(
      show_me_how_drunk(
        age = 68,
        sex = "male",
        height = 169,
        weight = 84,
        drinking_time = as.POSIXct(c("2016-10-03 08:10:00",
                                     "2016-10-03 08:10:00")),
        drinks = c("schnaps" = 3)
      )
    )
  )
  expect_true(
    ggplot2::is.ggplot(
      show_me_how_drunk(
        age = 68,
        sex = "male",
        height = 169,
        weight = 84,
        drinking_time = as.POSIXct(c("2016-10-03 08:10:00",
                                     "2016-10-03 08:11:00")),
        drinks = c("schnaps" = 3)
      )
    )
  )
})

test_that("Correct errors", {
  expect_error(
    show_me_how_drunk(
      age = 68,
      sex = "male",
      height = 169,
      weight = 84,
      drinking_time = as.POSIXct(c("2016-10-03 08:15:00",
                                   "2016-10-03 08:11:00")),
      drinks = c("schnaps" = 3)
    )
  )
  expect_error(
    show_me_how_drunk(
      age = 68,
      sex = "male",
      height = 169,
      weight = 84,
      drinking_time = c("2016-10-03 08:15:00",
                        "2016-10-03 08:11:00"),
      drinks = c("schnaps" = 3)
    )
  )
  expect_error(
    show_me_how_drunk(
      age = 6111,
      sex = "male",
      height = 169,
      weight = 84,
      drinking_time = as.POSIXct(c("2016-10-03 08:15:00",
                                   "2016-10-03 08:11:00")),
      drinks = c("schnaps" = 3)
    )
  )
})
