# ---- promillerechner-errors ----
# These should all trigger errors in YOUR **input checks**,
# either from match.arg or some assert_XXX function.
# (not just somewhere in your code....):

tell_me_how_drunk(
  age = NA,
  sex = "m",
  height = 190,
  weight = 87,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("schnaps" = 3)
)

tell_me_how_drunk(
  age = 45,
  sex = "unicorn",
  height = 190,
  weight = 87,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("schnaps" = 3)
)

tell_me_how_drunk(
  age = 82,
  sex = "f",
  height = -19,
  weight = 87,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("schnaps" = 3)
)

tell_me_how_drunk(
  age = 82,
  sex = "f",
  height = NULL,
  weight = 87,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("schnaps" = 3)
)


tell_me_how_drunk(
  age = 82,
  sex = "f",
  height = Inf,
  weight = 87,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("schnaps" = 3)
)

tell_me_how_drunk(
  age = 17,
  sex = "unicorn",
  height = 19,
  weight = c(60, 70),
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("schnaps" = 3)
)

tell_me_how_drunk(
  age = 17,
  sex = "unicorn",
  height = -19,
  weight = "fat",
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("schnaps" = 3)
)

tell_me_how_drunk(
  age = 14,
  sex = "f",
  height = 160,
  weight = 54,
  drinking_time = c("14.00", "21.00"),
  drinks = c("schnaps" = 4)
)

tell_me_how_drunk(    
  age = 24,
  sex = "f",
  height = 160,
  weight = 54,
  drinking_time = as.POSIXct(c("2016-10-03 21:00:00", "2016-10-03 14:00:00")),
  drinks = c("massn" = 4)
)


tell_me_how_drunk(
  age = 14,
  sex = "m",
  height = 160,
  weight = 54,
  drinking_time = as.POSIXct(c("2016-10-03 14:00:00", "2016-10-03 21:00:00")),
  drinks = c("schnaps" = -4)
)


tell_me_how_drunk(
  age = 34,
  sex = "m",
  height = 190,
  weight = 87,
  drinking_time = as.POSIXct(c("2016-10-03 18:15:00", "2016-10-03 22:55:00")),
  drinks = c("colaweizen" = 3)
)

tell_me_how_drunk(
  age = 24,
  sex = "f",
  height = 160,
  weight = 54,
  drinking_time = as.POSIXct(c("2016-10-03 14:00:00", "2016-10-03 21:00:00")),
  drinks = c(4, 2)
)

tell_me_how_drunk(
  age = 24,
  sex = "f",
  height = 160,
  weight = 54,
  drinking_time = as.POSIXct(c("2016-10-03 14:00:00", "2016-10-03 21:00:00")),
  drinks = list("wein" = NULL)
)

